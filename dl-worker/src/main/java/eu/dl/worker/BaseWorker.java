package eu.dl.worker;

import com.rabbitmq.client.AMQP;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Consumer;
import com.rabbitmq.client.DefaultConsumer;
import com.rabbitmq.client.Envelope;
import eu.dl.core.RecoverableException;
import eu.dl.core.UnrecoverableException;
import eu.dl.core.config.Config;
import eu.dl.core.config.MisconfigurationException;
import eu.dl.dataaccess.dao.TransactionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.ThreadContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.UUID;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Provides encapsulated functionality to all workers. This class knows how to
 * connect to the messaging system, mongo etc. and "shares" the corresponding
 * methods with its subclasses. All workers should extend this class.
 *
 * @author Kuba Krafka
 */
public abstract class BaseWorker extends RabbitMessenger implements Worker {

    /**
     * Main logger.
     */
    protected final Logger logger = LoggerFactory.getLogger(this.getClass().getName());

    /**
     * This value symbolizes latest version.
     */
    protected static final String LATEST = "latest";

    /**
     * Application config instance.
     */
    protected final Config config;

    /**
     * Number of displayed characters of message on debug level.
     */
    private static final int RAW_MESSAGE_LONG = 200;

    /**
     * Maximum number of attempts to resend message to queue when recovery exception is emitted.
     * E.g. 4 means one message can be in queue up to 5 times.
     */
    private int retryCountLimit;

    /**
     * Default maximum number of attempts to resend message to queue when recovery exception is emitted.
     */
    private static final int RETRY_COUNT_DEFAULT_LIMIT = 1;

    /**
     * Environment prefix used to define queue names etc.
     */
    private static String envPrefix;

    private static final Pattern SOURCE_ID_REGEX =
        Pattern.compile("^(?<prefix>([^\\.]+\\.)+worker(\\.[a-z]{2})?)\\.([^\\.]+\\.)+((?<worker>.+)(Tender|Body).+$)?");

    /**
     * Initialization common for all the workers. Registers worker in the
     * messaging, reads configuration variables, prepares logging.
     */
    protected BaseWorker() {
        super();

        config = Config.getInstance();
        envPrefix = config.getEnvId() + "_";
        retryCountLimit = RETRY_COUNT_DEFAULT_LIMIT;

        logger.debug("BaseWorker initialised");
    }

    /**
     * Gets the class name used in to identify the product of work.
     *
     * @return class name
     */
    protected final String getName() {
        return this.getClass().getName();
    }

    /**
     * Gets source ID used in to identify the source (czech source, TED source, ...).
     * E.g. "CZ" or "EU"
     *
     * @return source ID
     */
    protected final String getSourceId() {
        String sourceId = null;

        Matcher m = SOURCE_ID_REGEX.matcher(getName());
        if (m.find()) {
            sourceId = config.getParam(m.group("prefix") + "." + m.group("worker") + ".sourceId");
            if (sourceId == null) {
                sourceId = config.getParam(m.group("prefix") + ".sourceId");
            }
        }

        if (StringUtils.isBlank(sourceId)) {
            logger.error("There is no source ID defined for the worker {}", getName());
            throw new MisconfigurationException("There is no source ID defined for the worker");
        }

        return sourceId;
    }

    /**
     * Gets the version of the worker.
     *
     * @return worker version
     */
    protected abstract String getVersion();

    /**
     * Handles the actual piece of work the worker is supposed to perform. The
     * work is handed over in a form of message.
     *
     * @param message
     *            contains data necessary to do the work
     *
     * @throws UnrecoverableException
     *             in case the work cannot be done (even by another worker),
     *             because the message is corrupted etc.
     * @throws RecoverableException
     *             in case another worker may succeed and do the work
     */
    protected abstract void doWork(Message message);


    /**
     * Resends all the messages ever being send by this worker name and version.
     *
     * @param version
     *            version of the worker for which should be messages
     *            regenerated, use "latest" to resend latest available
     * @param dateFrom
     *            resend records modified from this date
     * @param dateTo
     *            resend records modified to this date
     * @throws UnrecoverableException
     *         in case the work cannot be done (even by another worker),
     *         because the message is corrupted etc.
     * @throws RecoverableException
     *         in case another worker may succeed and do the work
     */
    protected abstract void resend(String version, String dateFrom, String dateTo);

    /**
     * Returns utils used to manage transaction. Transaction utils are an
     * abstraction providing transaction commit, begin, rollback and similar.
     * 
     * @return transaction utils
     */
    protected abstract TransactionUtils getTransactionUtils();

    @Override
    public final void startWork() {
        // initialize messaging and start to consume and work on the tasks
        // delivered
        connectOutgoingExchange();
        connectIncomingQueue();
    }

    /**
     * Registers itself to receive messages from the queue. This methods also
     * handles error states when message is being processed. The queue is
     * defined here (and created if it doesn't exist).
     */
    private void connectIncomingQueue() {
        final Channel channel = getIncomingChanel();

        // message consumer
        final Consumer consumer = new DefaultConsumer(channel) {
            @Override
            public void handleDelivery(final String consumerTag, final Envelope envelope,
                                       final AMQP.BasicProperties properties, final byte[] body) throws IOException {
                Message message = MessageFactory.getMessage();
                try {
                    ThreadContext.put("message_id", UUID.randomUUID().toString());
                    ThreadContext.put("environment", envPrefix);
                    ThreadContext.put("worker_name", getName());
                    ThreadContext.put("worker_version", getVersion());

                    logger.info("Processing message");

                    // convert bytes to string
                    final String rawMessage = new String(body, "UTF-8");
                    logger.debug("Raw message: {}", StringUtils.abbreviate(rawMessage, RAW_MESSAGE_LONG));
                    logger.trace("Raw message: {}", rawMessage);

                    // parse incoming json to hashmap
                    message = MessageFactory.getMessage(rawMessage);

                    logger.debug("Parsed message {}", message);

                    // check whether special command was send
                    final String command = message.getValue("command");

                    // starting transactinon if needed
                    getTransactionUtils().begin();

                    if (command != null) {
                        if (command.equals("resend")) {
                            final String version = message.getValue("version");
                            final String fromDate = message.getValue("fromDate");
                            final String toDate = message.getValue("toDate");

                            if (version != null) {
                                logger.info("Triggering resend command with version {}", version);
                                resend(version, fromDate, toDate);
                            } else {
                                logger.info("No version specified in the message, sending \"latest\" instead.");
                                resend(LATEST, fromDate, toDate);
                            }
                        } else {
                            logger.error("Uknown command {}", command);
                            throw new UnrecoverableException("Unknow command in the message ");
                        }
                    } else {
                        // no special command send, process message
                        doWork(message);
                    }

                    // all work done, commit transaction
                    getTransactionUtils().commit();

                    logger.debug("Processed, sending ack back");

                    // acknowledge message - it will not be processed by next worker
                    channel.basicAck(envelope.getDeliveryTag(), false);
                    logger.info("Message processed. Waiting for next work...");
                } catch (final RecoverableException ex) {
                    getTransactionUtils().rollback();
                    ThreadContext.put("original_message", message.toJson());
                    ThreadContext.put("exception", ex.toString());
                    ThreadContext.put("stack_trace", stackTraceToString(ex));

                    logger.error("Recoverable exception thrown while doing work {}", ex);
                    logger.error("Message body: {}", message.toJson());

                    String retryCountString = message.getValue("retryCount");
                    int retryCount = (retryCountString == null) ? 0 : Integer.parseInt(retryCountString);
                    if (retryCount < getRetryCountLimit()) {
                        // acknowledge the message, increment the "retryCount" and publish it as new message.
                        // It will be tried again at the end of queue

                        channel.basicAck(envelope.getDeliveryTag(), false);
                        logger.error("Message was acknowledged and will be planned to requeue", ex);
                        message.setValue("retryCount", Integer.toString(retryCount + 1));

                        logger.info("Publishing json message to {}", envPrefix + getIncomingQueueName());
                        logger.debug("Message body: {}",
                                StringUtils.abbreviate(message.toJson(), RAW_MESSAGE_LONG));
                        logger.trace("Message body: {}", message.toJson());
                        channel.basicPublish("",
                                             envPrefix + getIncomingQueueName(),
                                             null,
                                             message.toJson().getBytes());
                    } else {
                        getTransactionUtils().rollback();
                        // not acknowledge the message and don't allow requeue
                        channel.basicNack(envelope.getDeliveryTag(), false, false);
                        logger.warn(
                                "Recoverable exception has been thrown {} times for this message (limit is "
                                        +
                                "{}) and the message WILL NOT BE REQUEUED",
                                retryCount + 1, getRetryCountLimit() + 1);
                    }
                } catch (final UnrecoverableException ex) {
                    // add info to thread context to store data in logs
                    ThreadContext.put("original_message", message.toJson());
                    ThreadContext.put("exception", ex.toString());
                    ThreadContext.put("stack_trace", stackTraceToString(ex));

                    logger.error("Unrecoverable exception thrown while doing work {}", ex);
                    logger.error("Message body: {}", message.toJson());
                    // not acknowledge the message
                    // and don't allow requeue

                    getTransactionUtils().rollback();

                    channel.basicNack(envelope.getDeliveryTag(), false, false);
                    logger.error("Message wasn't acknowledged and WILL NOT BE REQUEUED");
                } catch (final Exception ex) {
                    // add info to thread context to store data in logs
                    ThreadContext.put("original_message", message.toJson());
                    ThreadContext.put("exception", ex.toString());
                    ThreadContext.put("stack_trace", stackTraceToString(ex));

                    logger.error("General exception thrown while doing work {}", ex);
                    logger.error("Message body: {}", message.toJson());

                    // not acknowledge the message
                    // and don't allow requeue

                    getTransactionUtils().rollback();

                    channel.basicNack(envelope.getDeliveryTag(), false, false);
                    logger.error("Message wasn't acknowledged and WILL NOT BE REQUEUED");
                }

                getTransactionUtils().close();
                ThreadContext.clearAll();
            }
        };

        connectIncomingQueue(consumer, channel);
    }

    /**
     * Gets maximum number of attempts to resend message to queue when recovery exception is emitted.
     *
     * @return maximum number of attempts to resend message to queue when recovery exception is emitted
     */
    protected final int getRetryCountLimit() {
        return retryCountLimit;
    }

    /**
     * Sets maximum number of attempts to resend message to queue when recovery exception is emitted.
     *
     * @param retryCountLimit
     *         maximum number of attempts to resend message to queue when recovery exception is emitted
     */
    protected final void setRetryCountLimit(final int retryCountLimit) {
        this.retryCountLimit = retryCountLimit;
    }
    
    /**
     * Convert the result of Exception.getStackTrace to a String.
     * 
     * @param ex exception
     * @return stacktrace
     */
    private String stackTraceToString(final Exception ex) {
        String result = ex.toString() + "\n";
        StackTraceElement[] trace = ex.getStackTrace();
        for (int i=0; i < trace.length; i++) {
            result += trace[i].toString() + "\n";
        }
        return result;
    }
}
