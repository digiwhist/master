package eu.dl.worker.master;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.MasterDAO;
import eu.dl.dataaccess.dao.MatchedDAO;
import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.master.Masterable;
import eu.dl.dataaccess.dto.matched.MasterablePart;
import eu.dl.dataaccess.dto.matched.Matchable;
import eu.dl.worker.BaseWorker;
import eu.dl.worker.Message;
import eu.dl.worker.indicator.plugin.IndicatorPlugin;
import eu.dl.worker.master.plugin.MasterPlugin;
import eu.dl.worker.utils.BasicPluginRegistry;
import eu.dl.worker.utils.PluginRegistry;
import org.apache.logging.log4j.ThreadContext;

import java.util.ArrayList;
import java.util.List;
import java.util.Map.Entry;
import java.util.stream.Collectors;

/**
 * Base class for all the masters.
 *
 * @param <T>
 *            matched items
 * 
 * @param <V>
 *            item to be mastered
 */
public abstract class BaseMaster<T extends Matchable & MasterablePart, V extends Masterable> extends BaseWorker {
    private static final String OUTGOING_EXCHANGE_NAME = "master";

    private static final String INCOMING_EXCHANGE_NAME = "matched";

    protected PluginRegistry<MasterPlugin> pluginRegistry = new BasicPluginRegistry<>();

    protected PluginRegistry<IndicatorPlugin<V>> indicatorPluginRegistry = new BasicPluginRegistry();

    private final MatchedDAO<T> matchedDAO = getMatchedDAO();

    private final MasterDAO<V> masterDAO = getMasterDAO();

    /**
     * Time execution threshold of this master worker (in milliseconds).
     */
    private static final long WORKER_TIME_THRESHOLD = 1000;

    /**
     * Initializes this class to be used. Registers plugins etc.
     */
    public BaseMaster() {
        super();
        registerCommonPlugins();
        registerProjectSpecificPlugins();
        registerSpecificPlugins();
        registerIndicatorPlugins();
    }

    @Override
    protected final void doWork(final Message message) {
        long startTime = System.currentTimeMillis();

        getTransactionUtils().begin();
        final String groupId = message.getValue("groupId");
        ThreadContext.put("group_id", groupId);

        // get the matched items
        long selectStartTime = System.currentTimeMillis();
        List<T> matchedItems = matchedDAO.getByGroupId(groupId);
        List<T> rawMatchedItems = matchedItems.stream().collect(Collectors.toList());

        long selectEndTime = System.currentTimeMillis();
        logger.info("Selection of matched objects for {} took {} ms.", this.getClass().getName(),
                    selectEndTime - selectStartTime);

        // get already existing items(result of previous mastering)
        List<V> existingItems = masterDAO.getByGroupId(groupId);

        // check whether there is only one item and prepare this item to be updated
        V item = masterDAO.getEmptyInstance();
        
        if (existingItems.size() > 1) {
            logger.error("There are more({}) mastered instances with the same groupId {}.", existingItems.size(),
                    groupId);
            throw new UnrecoverableException("There are more mastered instances with the same groupId.");
        } else if (!existingItems.isEmpty()) {
            item = existingItems.get(0);
        }

        item.setPersistentId(getPersistentId(matchedItems));
        item.setProcessingOrder(getProcessingOrder(matchedItems));
        // preprocess matched items before creating master
        matchedItems = generalPreprocessData(matchedItems);
        if (matchedItems.isEmpty()) {
            logger.info("No items left for mastering after the general preprocessing.");
            return;
        }
        matchedItems = sourceSpecificPreprocessData(matchedItems);
        if (matchedItems.isEmpty()) {
            logger.info("No items left for mastering after the source specific preprocessing.");
            return;
        }

        // iterate over all plugins and execute them in a proper order
        for (Entry<String, MasterPlugin> entry : pluginRegistry.getPlugins().entrySet()) {
            MasterPlugin<T, V, T> plugin = entry.getValue();
            item = plugin.master(matchedItems, item, matchedItems);
        }

        // save master record
        item.setGroupId(groupId);
        
        item = postProcessMasterRecord(item, rawMatchedItems);
                
        item = sourceSpecificPostprocessData(item);

        List<Indicator> indicators = new ArrayList<>();
        
        // iterate over all indicator plugins and execute them in a proper order
        for (Entry<String, IndicatorPlugin<V>> entry : indicatorPluginRegistry.getPlugins().entrySet()) {
            IndicatorPlugin<V> plugin = entry.getValue();
            Indicator indicator = plugin.evaluate(item);
            if (indicator != null) {
            		indicators.add(indicator);
            }
        }

        item.setIndicators(indicators);

        String savedId = masterDAO.save(item);

        
        getTransactionUtils().commit();
        logger.info("Mastering finished for group id {} stored as {}", groupId, savedId);

        long endTime = System.currentTimeMillis();
        if ((endTime - startTime) > WORKER_TIME_THRESHOLD) {
            logger.warn("Execution of master worker took {} ms.", endTime - startTime);
        }
    }

    /**
     * This method is used for master item postProcessing before its saved to persistent storage. 
     * There can be additional values calculated etc.
     *  
     * @param item master item
     * @param matchedItems matched items
     *
     * @return post processed item
     */
    protected abstract V postProcessMasterRecord(V item, List<T> matchedItems);

    /**
     * Creates persistent ID for this item set. 
     * @param matchedItems set of items
     * @return persistent id 
     */
    protected abstract String getPersistentId(List<T> matchedItems);

    /**
     * Creates processing order as the minimal available.
     * @param matchedItems set of items
     * @return persistent id
     */
    private String getProcessingOrder(final List<T> matchedItems) {
        if (matchedItems == null || matchedItems.isEmpty()) {
            return null;
        }

        String min = null;
        for (T item : matchedItems) {
            if (item.getProcessingOrder() != null && (min == null || min.compareTo(item.getProcessingOrder()) < 0)) {
                min = item.getProcessingOrder();
            }
        }

        return min;
    }

    @Override
    protected final void resend(final String version, final String dateFrom, final String dateTo) {
        throw new UnrecoverableException("Master worker does not support message resending.");
    }

    @Override
    protected final String getOutgoingExchangeName() {
        return OUTGOING_EXCHANGE_NAME;
    }

    @Override
    protected final String getIncomingExchangeName() {
        return INCOMING_EXCHANGE_NAME;
    }

    /**
     * Returns dao instance used to handle matched results.
     *
     * @return matched dao
     */
    protected abstract MatchedDAO<T> getMatchedDAO();

    /**
     * Returns dao instance used to handle master results.
     *
     * @return matched dao
     */
    protected abstract MasterDAO<V> getMasterDAO();

    /**
     * This function registers common mastering plugins. The plugins will
     * be used for deduplication in all the masters. Common plugins are executed
     * in FIFO order before the source specific ones.
     */
    protected abstract void registerCommonPlugins();

    /**
     * Registers project specific plugins to be used for mastering.
     */
    protected abstract void registerProjectSpecificPlugins();

    /**
     * Registers worker specific plugins to be used for mastering.
     */
    protected abstract void registerSpecificPlugins();

    /**
     * Registers worker specific plugins to be used for mastering.
     */
    protected abstract void registerIndicatorPlugins();

    /**
     * The data can be preprocessed(filtered, info added etc.) in this method.
     * Should be implemented in base for multiple workers.
     *
     * @param items
     *         data to be preprocessed
     *
     * @return preprocessed data
     */
    protected abstract List<T> generalPreprocessData(List<T> items);

    /**
     * The data can be preprocessed(filtered, info added etc.) in this method.
     * Should be implemented in specific workers.
     *
     * @param items
     *         data to be preprocessed
     *
     * @return preprocessed data
     */
    protected abstract List<T> sourceSpecificPreprocessData(List<T> items);

    /**
     * The master record can be postprocessed in this method.
     * Should be implemented in specific workers.
     *
     * @param item
     *         master item to be postprocessed
     *
     * @return postprocessed master item
     */
    protected abstract V sourceSpecificPostprocessData(V item);
}
