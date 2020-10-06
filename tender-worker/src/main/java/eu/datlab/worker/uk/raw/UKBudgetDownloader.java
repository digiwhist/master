package eu.datlab.worker.uk.raw;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.RawDataDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.raw.Raw;
import eu.dl.worker.Message;
import eu.dl.worker.MessageFactory;
import eu.dl.worker.raw.BaseRawWorker;
import eu.dl.worker.raw.utils.DownloaderUtils;
import eu.dl.worker.utils.textFactory.PlainTextService;
import org.jsoup.Connection;

import java.io.ByteArrayInputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;

import static eu.dl.worker.raw.utils.DownloaderUtils.generatePersistentId;

/**
 * Download for UK budgets in PDF.
 */
public final class UKBudgetDownloader extends BaseRawWorker {

    private static final String VERSION = "1.0";

    private final RawDataDAO dao = DAOFactory.getDAOFactory().getRawBudgetItemDAO(getName(), getVersion());

    @Override
    public void doWork(final Message message) {
        // get url from message
        final String url = message.getValue("url");
        Raw rawData = dao.getEmptyInstance();

        try {
            rawData.setSourceUrl(new URL(url));
        } catch (final MalformedURLException ex) {
            logger.error("Unable to download from malformed URL {}", url);
            throw new UnrecoverableException("Unable to download data because of malformed url", ex);
        }

        final Connection.Response response = DownloaderUtils.getUrlResponse(url);
        logger.info("Downloaded data from {}", url);

        logger.info("Parsing downloaded data from PDF to Text {}");
        rawData.setSourceData(PlainTextService.parseTextFrom(new ByteArrayInputStream(DownloaderUtils
                .getUrlResponse(url).bodyAsBytes())));
        rawData.setSourceDataMimeType(response.contentType());

        logger.info("Parsed downloaded data from PDF to Text {}");

        if (rawData.getPersistentId() == null) {
            rawData.setPersistentId(generatePersistentId(rawData, getSourceId()));
        }

        rawData.setMetaData(message.getMetaData());

        final String savedId = dao.save(rawData);
        getTransactionUtils().commit();
        logger.info("Stored raw data as {}", savedId);
        // create and publish message with saved id
        final Message outgoingMessage = MessageFactory.getMessage();
        outgoingMessage.setValue("id", savedId);
        publishMessage(outgoingMessage);
    }

    @Override
    public String getVersion() {
        return VERSION;
    }

    @Override
    protected String getIncomingQueueName() {
        return getIncomingQueueNameFromConfig();
    }

    @Override
    protected String getIncomingExchangeName() {
        return "raw";
    }

    @Override
    protected void resend(final String version, final String dateFrom, final String dateTo) {
        logger.debug("Resending messages to be parsed.");

        try {
            String resendVersion = version;
            if (version.equals(LATEST)) {
                // current version data should be resent
                resendVersion = getVersion();
            }

            final List<Raw> rawDataItems = dao.getMine(getName(), resendVersion, dateFrom, dateTo);

            for (final Raw rawDataItem : rawDataItems) {
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
    protected TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }
}
