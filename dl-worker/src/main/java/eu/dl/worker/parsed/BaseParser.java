package eu.dl.worker.parsed;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.ParsedDAO;
import eu.dl.dataaccess.dao.RawDAO;
import eu.dl.dataaccess.dto.parsed.Parsable;
import eu.dl.dataaccess.dto.raw.Raw;
import eu.dl.worker.BaseWorker;
import eu.dl.worker.Message;
import eu.dl.worker.MessageFactory;
import org.apache.logging.log4j.ThreadContext;

import java.time.format.DateTimeFormatter;
import java.util.List;

/**
 * Base class for all the parsers.
 * 
 * @param <T>
 *            parsable item
 * 
 * @param <V>
 *            raw item
 */
public abstract class BaseParser<V extends Raw, T extends Parsable> extends BaseWorker {

    private static final String OUTGOING_EXCHANGE_NAME = "parsed";

    private static final String INCOMING_EXCHANGE_NAME = "raw";

    private final RawDAO<V> rawDao = getRawDAO();

    private final ParsedDAO<T> parsedDao = getParsedDAO();

    @Override
    public final void doWork(final Message message) {
        getTransactionUtils().begin();
        final String rawItemId = message.getValue("id");
        ThreadContext.put("raw_tender_id", rawItemId);
        final V rawItem = rawDao.getById(rawItemId);

        final List<T> parsedItems = parse(rawItem);
        logger.debug("Number of tenders parsed: {}", parsedItems.size());

        // set raw ids to parsed items
        for (T parsedItem : parsedItems) {
            parsedItem.setRawObjectId(rawItemId);
        }

        final List<T> processedParsedItems = postProcess(parsedItems, rawItem);

        int counter = 1;
        String rawPersistentId = rawItem.getPersistentId();
        
        // send messages about processed items
        for (T parsedTender : processedParsedItems) {
            parsedTender.setRawObjectId(rawItemId);
            
            // generate persistent id
            if (rawPersistentId != null) {
                parsedTender.setPersistentId(rawPersistentId + "_" + counter);
                counter++;
            }

            // set item processing order
            if (rawItem.getCreated() != null) {
                DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSSSSSSS");
                parsedTender.setProcessingOrder(rawItem.getCreated().format(formatter));
            }

            getTransactionUtils().begin();
            final String parsedId = parsedDao.save(parsedTender);
            getTransactionUtils().commit();
            createAndPublishMessage(parsedId);
        }

    }

    @Override
    public final void resend(final String version, final String dateFrom, final String dateTo) {
        logger.debug("Resending messages to be cleaned.");

        try {
            String resendVersion = version;
            if (version.equals(LATEST)) {
                // current version data should be resent
                resendVersion = getVersion();
            }

            final List<T> items = parsedDao.getMine(getName(), resendVersion, dateFrom, dateTo);

            for (final Parsable item : items) {
                final Message outgoingMessage = MessageFactory.getMessage();
                outgoingMessage.setValue("id", item.getId());
                publishMessage(outgoingMessage);
            }
        } catch (final Exception ex) {
            logger.error("Unable to resend messages for cleaning {}", ex);
            throw new UnrecoverableException("Unable to resend messages for cleaning", ex);
        }
    }

    /**
     * Returns specific raw item DAO instance.
     *
     * @return dao instance for storing raw item to database
     */
    protected abstract RawDAO<V> getRawDAO();

    /**
     * Returns specific parsed item DAO instance.
     *
     * @return dao instance for storing parsed items to database
     */
    protected abstract ParsedDAO<T> getParsedDAO();

    /**
     * Parses the given raw item object.
     *
     * @param raw
     *            raw item to be parsed
     *
     * @return list of parsed items or empty list if none items have been parsed
     * @throws UnrecoverableException
     *             if no parsed could be created for given raw item
     */
    public abstract List<T> parse(V raw);

    /**
     * Post processing of parsed items. For example lot IDs are generated here.
     *
     * @param parsed
     *            list of parsed items to be post processed
     * @param raw
     *            raw item to be parsed 
     * @return list of post processed items
     */
    protected abstract List<T> postProcess(List<T> parsed, V raw);

    /**
     * Publishes new message with id of saved parsed entity (eg. tender,
     * contracting authority) to outgoing queue.
     *
     * @param savedId
     *            id of the saved entity (eg. tender, contracting authority)
     */
    public final void createAndPublishMessage(final String savedId) {
        final Message outgoingMessage = MessageFactory.getMessage();
        outgoingMessage.setValue("id", savedId);
        logger.debug("Parsing finished, publishing message {}", outgoingMessage);
        publishMessage(outgoingMessage);
    }

    @Override
    protected final String getOutgoingExchangeName() {
        return OUTGOING_EXCHANGE_NAME;
    }

    @Override
    protected final String getIncomingExchangeName() {
        return INCOMING_EXCHANGE_NAME;
    }

    @Override
    protected final String getIncomingQueueName() {
        return getIncomingQueueNameFromConfig();
    }
}
