package eu.dl.worker.raw.downloader;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.RawDAO;
import eu.dl.dataaccess.dto.raw.Raw;
import eu.dl.worker.Message;
import eu.dl.worker.MessageFactory;
import eu.dl.worker.raw.BaseRawWorker;
import eu.dl.worker.utils.ThreadUtils;

import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;

import static eu.dl.worker.raw.utils.DownloaderUtils.generatePersistentId;

/**
 * Provides basic functionality for data download from given url.
 * 
 * @param <T>
 *            item to be downloaded
 */
public abstract class BaseDownloader<T extends Raw> extends BaseRawWorker {

    private static final String INCOMING_EXCHANGE_NAME = "raw";

    protected RawDAO<T> rawDao;

    protected final int humanize;

    protected final boolean skipExisting;

    /**
     * Default constructor.
     */
    protected BaseDownloader() {
        super();        
        rawDao = getRawDataDao();

        String humanizeProperty = this.getClass().getName() + ".humanize";
        String humanizeValue = config.getParam(humanizeProperty);
        if (humanizeValue != null) {
            try {
                humanize = Integer.valueOf(humanizeValue);
            } catch (final NumberFormatException ex) {
                logger.error("Value '{}' of property {} isn't an integer", humanizeValue, humanizeProperty);
                throw new UnrecoverableException("Configuration property must be an integer", ex);
            }
        } else {
            humanize = 0;
        }

        if (config.getParam(getName() + ".skipExisting") != null) {
            skipExisting = config.getParam(getName() + ".skipExisting").equals("1");
        } else {
            skipExisting = false;
        }
    }

    @Override
    public final void doWork(final Message message) {
        logger.debug("Doing work for message {}", message);

        if (skipExisting && skipExisting(message)) {
            logger.debug("Raw data for message '{}' are already downloaded", message.toJson());
            return;
        }

        if (humanize > 0) {
            ThreadUtils.sleep(humanize);
        }

        // download and populate raw data (there might me more records at once => list of raw data objects)
        final List<T> rawData = downloadAndPopulateRawData(message);

        // save all the stuff
        for (T rawDataItem : rawData) {

            // generate persistent id if not already set by the worker logic
            if (rawDataItem.getPersistentId() == null) {
                rawDataItem.setPersistentId(generatePersistentId(rawDataItem, getSourceId()));
            }

            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSSSSSSS");
            rawDataItem.setProcessingOrder(Timestamp.valueOf(LocalDateTime.now()).toLocalDateTime().format(formatter));
            final String savedId = rawDao.save(rawDataItem);
            logger.info("Stored raw data as {}", savedId);

            // post-processing, doesn't affect raw record
            postProcess(rawDataItem);

            // create and publish message with saved id
            final Message outgoingMessage = MessageFactory.getMessage();
            outgoingMessage.setValue("id", savedId);
            publishMessage(outgoingMessage);
        }
    }

    /**
     * Checks whether skip incoming message. Method is taking effect only in the case of enabled skipExisting.
     *
     * @param message
     *      incoming message
     * @return TRUE for skip, otherwise FALSE
     */
    protected abstract boolean skipExisting(Message message);

    @Override
    public final void resend(final String version, final String dateFrom, final String dateTo) {
        logger.debug("Resending messages to be parsed.");

        try {
            String resendVersion = version;
            if (version.equals(LATEST)) {
                // current version data should be resent
                resendVersion = getVersion();
            }

            final List<T> rawDataItems = getRawDataDao().getMine(getName(), resendVersion, dateFrom, dateTo);

            for (final T rawDataItem : rawDataItems) {
                final Message outgoingMessage = MessageFactory.getMessage();
                outgoingMessage.setValue("id", rawDataItem.getId());
                publishMessage(outgoingMessage);
            }
        } catch (final Exception ex) {
            logger.error("Unable to resend messages for parsing {}", ex);
            throw new UnrecoverableException("Unable to resend messages for parsing", ex);
        }
    }

    @Override
    protected final String getIncomingExchangeName() {
        return INCOMING_EXCHANGE_NAME;
    }
    
    /**
     * Processes message from crawler, downloads requested source data and
     * prepares raw data object.
     *
     * @param message
     *            message from crawler containing parameters for downloading
     *
     * @return raw data objects populated with downloaded data
     */
    public abstract List<T> downloadAndPopulateRawData(Message message);

    /**
     * Returns specific DAO instance according to downloader type (tender, contracting authority etc.).
     *
     * @return dao instance for storing raw data to database
     */
    public abstract RawDAO<T> getRawDataDao();

    /**
     * Returns downloader version.
     *
     * @return downloader version
     */
    public abstract String getVersion();

    /**
     * Post-processing. Method doesn't (shouldn't) affect saved raw record, but gives the opportunity to do operations after record saving.
     *
     * @param raw
     *      saved raw record
     */
    protected abstract void postProcess(T raw);

    @Override
    protected final String getIncomingQueueName() {
        return getIncomingQueueNameFromConfig();
    }
}
