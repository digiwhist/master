package eu.dl.worker;

import com.rabbitmq.client.AlreadyClosedException;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.ConnectionFactory;
import com.rabbitmq.client.Consumer;
import com.rabbitmq.client.MessageProperties;
import eu.dl.core.UnrecoverableException;
import eu.dl.core.config.Config;
import eu.dl.core.config.MisconfigurationException;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.net.SocketException;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeoutException;

/**
 * Provides rabbit messaging functionality.
 *
 * @author Tomas Mrazek
 */
public abstract class RabbitMessenger {
    /**
     * Main logger.
     */
    protected final Logger logger = LoggerFactory.getLogger(this.getClass().getName());

    /**
     * Application config instance.
     */
    protected final Config config;

    /**
     * RabbitMQ channel for outgoing messages.
     */
    private Channel outgoingChannel;

    /**
     * Number of displayed characters of message on debug level.
     */
    private static final int RAW_MESSAGE_LONG = 200;

    /**
     * Newer versions of rabbitmq are significantly slower after sending 100 000 messages.
     */
    private static final int MAX_MESSAGES_SENT_BY_EXCHANGE_CONNECTION = 100000;
    private int messagesSentByExchangeConnection;

    /**
     * Environment prefix used to define queue names etc.
     */
    private static String envPrefix;

    private static final Integer PUBLISH_MESSAGE_FAILURE_LIMIT = 5;
    private static final long PUBLISH_MESSAGE_FAILURE_SLEEP_TIME = 60000;

    /**
     * Initialization common for all the workers. Registers worker in the
     * messaging, reads configuration variables, prepares logging.
     */
    protected RabbitMessenger() {
        config = Config.getInstance();
        envPrefix = config.getEnvId() + "_";
    }

    /**
     * Provides name of the incoming queue.
     *
     * @return name of the incoming queue
     */
    protected abstract String getIncomingQueueName();

    /**
     * Provides name of the incoming queue.
     *
     * @return name of the incoming queue
     */
    protected final String getIncomingQueueNameFromConfig() {
        final String workerName = this.getClass().getName();
        final String queueName = config.getParam(workerName + ".incomingQueue");

        if (StringUtils.isBlank(queueName)) {
            logger.error("There is no incoming queue configured for the worker {}", queueName);
            throw new MisconfigurationException("There is no incoming queue configured for the worker");
        }

        return queueName;
    }

    /**
     * Provides name of the outgoing queue. The worker usually publishes into
     * the queue identified by its fully qualified class name. Tag can be set with a configuration property
     * {@code <worker_class>.outgoingTag}.
     *
     * @return name of the outgoing queue
     */
    protected final String getOutgoingTag() {
        String outgoingTag = config.getParam(this.getClass().getName() + ".outgoingTag");
        return StringUtils.isBlank(outgoingTag) ? this.getClass().getName() : outgoingTag;
    }

    /**
     * Provides name of the outgoing exchange. The worker usually publishes into
     * the exchange identified by its stage "raw", "parsed" etc.
     *
     * @return name of the outgoing exchange
     */
    protected abstract String getOutgoingExchangeName();

    /**
     * Provides name of the incoming exchange. The worker usually reads from the
     * exchange of previous stage a.k.a. "parsed from "raw".
     * 
     * @return name of the incoming exchange
     */
    protected abstract String getIncomingExchangeName();

    /**
     * Registers itself to receive messages from the queue. This methods also
     * handles error states when message is being processed. The queue is
     * defined here (and created if it doesn't exist).
     *
     * @param consumer
     *      rabbit channel consumer
     * @param channel
     *      rabbit channel
     */
    protected final void connectIncomingQueue(final Consumer consumer, final Channel channel) {
        try {
            logger.info("Connected incoming exchange '{}', established que '{}' with tag '{}'",
                envPrefix + getIncomingExchangeName(),
                envPrefix + getIncomingQueueName(),
                envPrefix + getIncomingQueueName());

            channel.basicConsume(envPrefix + getIncomingQueueName(), false, consumer);
        } catch (IOException ex) {
            logger.error("Unable to establish connection with messaging system", ex);
            throw new RuntimeException("Unable to establish connection with messaging system", ex);
        }
    }

    /**
     * Establishes connection with rabbit.
     *
     * @return connection
     */
    private Connection newConnection() {
        try {
            // configure connection
            final ConnectionFactory factory = new ConnectionFactory();
            factory.setHost(config.getParam("rabbitmq.host"));
            factory.setUsername(config.getParam("rabbitmq.username"));
            factory.setPassword(config.getParam("rabbitmq.password"));
            // factory.setPort(5674);

            // establish connection
            return factory.newConnection();
        } catch (IOException | TimeoutException ex) {
            logger.error("Unable to establish connection with messaging system - {}", ex);
            throw new RuntimeException("Unable to establish connection with messaging system", ex);
        }
    }

    /**
     * Gets channel for incoming queue.
     * 
     * @return initialised channel
     */
    protected final Channel getIncomingChanel() {
        try {
            // establish connection
            final Connection connection = newConnection();
            final Channel channel = connection.createChannel();

            // define the queue as lazy - store params on the disk
            Map<String, Object> args = new HashMap<>();
            args.put("x-queue-mode", "lazy");

            // declare the queue itself
            channel.queueDeclare(envPrefix + getIncomingQueueName(), true, false, false, args);
            channel.exchangeDeclare(envPrefix + getIncomingExchangeName(), "direct", true);
            channel.basicQos(1);

            channel.queueBind(envPrefix + getIncomingQueueName(),
                envPrefix + getIncomingExchangeName(),
                envPrefix + getIncomingQueueName());

            return channel;
        } catch (IOException ex) {
            logger.error("Unable to establish connection with messaging system - {}", ex);
            throw new RuntimeException("Unable to establish connection with messaging system", ex);
        }
    }

    /**
     * Registers itself to send messages to the queue. The queue is defined here
     * (and created if it doesn't exist yet).
     */
    protected final void connectOutgoingExchange() {
        try {
            logger.info("Initialised outgoing queue host:{} username:{} password:{}", config.getParam("rabbitmq.host"),
                config.getParam("rabbitmq.username"), config.getParam("rabbitmq.password"));

            // establish connection
            final Connection connection = newConnection();
            outgoingChannel = connection.createChannel();

            outgoingChannel.exchangeDeclare(envPrefix + getOutgoingExchangeName(), "direct", true);

            messagesSentByExchangeConnection = 0;
            logger.info("Connection to outgoing exchange {} established", envPrefix + getOutgoingExchangeName());
        } catch (IOException ex) {
            logger.error("Unable to establish connection with messaging system - {}", ex);
            throw new RuntimeException("Unable to establish connection with messaging system", ex);
        }
    }

    /**
     * Sends message to the outgoing exchange with default tag.
     *
     * @param message
     *            message to be send to other workers
     */
    protected final void publishMessage(final Message message) {        
        publishMessage(message, getOutgoingTag());
    }

    /**
     * Sends message to the outgoing exchange with a specified tag.
     *
     * @param message
     *            message to be send to other workers
     * @param tag
     *            tag for the message
     */
    protected final void publishMessage(final Message message, final String tag) {
        try {
            logger.debug("Message body: {}", StringUtils.abbreviate(message.toJson(), RAW_MESSAGE_LONG));
            logger.trace("Message body: {}", message.toJson());

            messagesSentByExchangeConnection++;
            if (messagesSentByExchangeConnection > MAX_MESSAGES_SENT_BY_EXCHANGE_CONNECTION) {
                outgoingChannel.abort();
                connectOutgoingExchange();
            }

            int exceptionsCount = 0;
            while (true) {
                try {
                    outgoingChannel.basicPublish(envPrefix + getOutgoingExchangeName(),
                            envPrefix + tag,
                            MessageProperties.PERSISTENT_TEXT_PLAIN,
                            message.toJson().getBytes());
                    break;
                } catch (SocketException | AlreadyClosedException e1) {
                    logger.error(e1.getClass().getName() + " exception occurred during message publish.", e1);
                    if (exceptionsCount > 0) {
                        throw new UnrecoverableException("Reconnection outgoing channel did not work");
                    }
                    while (true) {
                        try {
                            logger.error("Going to sleep for " + PUBLISH_MESSAGE_FAILURE_SLEEP_TIME + " ms.");
                            Thread.sleep(PUBLISH_MESSAGE_FAILURE_SLEEP_TIME);
                        } catch (InterruptedException ex) {
                            logger.error("Thread interrupted, waking up {}", ex);
                        }
                        try {
                            // reconnect outgoing channel
                            connectOutgoingExchange();
                            break;
                        } catch (RuntimeException e2) {
                            if (++exceptionsCount > PUBLISH_MESSAGE_FAILURE_LIMIT) {
                                throw new UnrecoverableException(
                                        "Unable to publish message. The worker have been waiting " +
                                                PUBLISH_MESSAGE_FAILURE_LIMIT * PUBLISH_MESSAGE_FAILURE_SLEEP_TIME +
                                                " ms with no success", e2);
                            }
                        }
                    }
                }
            }

            logger.info("Published json message to exchange {} , tag {}", envPrefix + getOutgoingExchangeName(),
                    envPrefix + tag);
        } catch (final IOException ex) {
            logger.error("Unable to publish message - {}", ex);
            throw new UnrecoverableException("Unable to publish message", ex);
        }
    }
}
