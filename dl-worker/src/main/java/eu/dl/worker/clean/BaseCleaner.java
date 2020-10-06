package eu.dl.worker.clean;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.CleanDAO;
import eu.dl.dataaccess.dao.ParsedDAO;
import eu.dl.dataaccess.dto.clean.Cleanable;
import eu.dl.dataaccess.dto.parsed.Parsable;
import eu.dl.worker.BaseWorker;
import eu.dl.worker.Message;
import eu.dl.worker.MessageFactory;
import eu.dl.worker.clean.plugin.CleaningPlugin;
import eu.dl.worker.utils.BasicPluginRegistry;
import eu.dl.worker.utils.PluginRegistry;
import org.apache.logging.log4j.ThreadContext;

import java.util.List;
import java.util.Map.Entry;

/**
 * This class covers the main functionality for the cleaners implementation.
 *
 * @param <T>
 *            parsable item
 * @param <V>
 *            cleanable item
 */
public abstract class BaseCleaner<T extends Parsable, V extends Cleanable> extends BaseWorker {
    private ParsedDAO<T> parsedDao;

    private CleanDAO<V> cleanDao;

    protected PluginRegistry<CleaningPlugin> pluginRegistry = new BasicPluginRegistry<CleaningPlugin>();

    private static final String OUTGOING_EXCHANGE_NAME = "clean";

    private static final String INCOMING_EXCHANGE_NAME = "parsed";

    /**
     * Default constructor.
     */
    protected BaseCleaner() {
        super();
        registerCommonPlugins();
        registerSpecificPlugins();
        parsedDao = getParsedDAO();
        cleanDao = getCleanDAO();
    }

    @Override
    protected final void doWork(final Message message) {
        try {
            getTransactionUtils().begin();
            final String id = message.getValue("id");
            ThreadContext.put("parsed_tender_id", id);
            logger.debug("Cleaning started for parsed tender {}", id);
            T parsedItem = parsedDao.getById(id);
            V cleanItem = cleanDao.getEmptyInstance();

            cleanItem.setPersistentId(parsedItem.getPersistentId());

            // set item processing order
            cleanItem.setProcessingOrder(parsedItem.getProcessingOrder());

            parsedItem = preProcessParsedItem(parsedItem);
            
            cleanItem.setRawObjectId(parsedItem.getRawObjectId());

            cleanItem.setParsedObjectId(parsedItem.getId());

            // iterate over all plugins and execute them in a proper order
            for (Entry<String, CleaningPlugin> entry : pluginRegistry.getPlugins().entrySet()) {
                CleaningPlugin<T, V> plugin = entry.getValue();
                cleanItem = plugin.clean(parsedItem, cleanItem);
                logger.debug("Tender cleaned with plugin {}", plugin);
            }

            cleanItem = postProcessCommonRules(cleanItem, parsedItem);
            cleanItem = postProcessSourceSpecificRules(parsedItem, cleanItem);
            // remove nonsenses
            cleanItem = (V) cleanItem.getValid();

            // save clean tender
            String savedId = cleanDao.save(cleanItem);
            getTransactionUtils().commit();
            logger.debug("Cleaning finished for parsed tender {}, stored as clean {}", id, savedId);

            // publish message
            final Message outgoingMessage = MessageFactory.getMessage();
            outgoingMessage.setValue("id", savedId);
            publishMessage(outgoingMessage);
            logger.info("Cleaning finished, published message: {}", outgoingMessage);
        } catch (final Exception e) {
            getTransactionUtils().rollback();
            logger.error("Cleaning failed with exception {}", e);
            throw new UnrecoverableException("Cleaning failed.", e);
        }
    }

    /**
     * This function registers common cleaning plugins. The plugins will be used
     * for clean in all the workers. Common plugins are executed in FIFO order
     * before the source specific ones.
     */
    protected abstract void registerCommonPlugins();

    /**
     * Register source specific cleaning plugins in this method. The plugins
     * will be executed in the FIFO order. Common plugins preceed the ones
     * registered here.
     */
    protected abstract void registerSpecificPlugins();

    /**
     * Returns actual version of this cleaner.
     *
     * @return actual cleaner version
     */
    public abstract String getVersion();

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

    @Override
    protected final void resend(final String version, final String dateFrom, final String dateTo) {
        logger.debug("Resending messages to be matched.");

        try {
            String resendVersion = version;
            if (version.equals(LATEST)) {
                // current version data should be resent
                resendVersion = getVersion();
            }

            final List<V> items = cleanDao.getMine(getName(), resendVersion, dateFrom, dateTo);

            for (final V item : items) {
                final Message outgoingMessage = MessageFactory.getMessage();
                outgoingMessage.setValue("id", item.getId());
                publishMessage(outgoingMessage);
            }
        } catch (final Exception ex) {
            logger.error("Unable to resend messages for matching {}", ex);
            throw new UnrecoverableException("Unable to resend messages for matching", ex);
        }
    }

    /**
     * Common rules post processing of clean item.
     * For example the selection method "lowest price" implies 100% price related criterion.
     *
     * @param cleanItem
     *            clean item to be post processed
     * @param parsedItem
     *            parsed item to be post processed
     * @return post processed clean item
     */
    protected abstract V postProcessCommonRules(V cleanItem, T parsedItem);

    /**
     * Source specific rules post processing of clean item.
     *
     * @param cleanItem
     *            clean item to be post processed
     * @param parsedItem
     *            parsed item to be post processed
     * @return post processed clean item
     */
    protected abstract V postProcessSourceSpecificRules(T parsedItem, V cleanItem);

    /**
     * Returns dao instance used to handle clean results.
     * 
     * @return clean dao
     */
    protected abstract CleanDAO<V> getCleanDAO();

    /**
     * Returns dao instance used to handle parsed results.
     * 
     * @return parsed dao
     */
    protected abstract ParsedDAO<T> getParsedDAO();

    /**
     * Pre processing of parsed item. Is useful in case if we need to edit data of parsed item before its cleaning.
     *
     * @param parsedItem
     *            parsed item to be pre processed
     * @return pre processed parsed item
     */
    protected abstract T preProcessParsedItem(T parsedItem);
}
