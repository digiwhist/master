package eu.datlab.worker.ch.raw;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.RawDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.Message;
import eu.dl.worker.raw.downloader.BaseDownloader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Arrays;
import java.util.List;
import static org.apache.commons.codec.digest.DigestUtils.sha256Hex;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

/**
 * SIMAP Tender downloader.
 */
public final class SimapTenderDownloader extends BaseDownloader<RawData> {

    /**
     * Worker version.
     */
    public static final String VERSION = "1.1";


    @Override
    protected boolean skipExisting(final Message message) {
        return false;
    }

    @Override
    public List<RawData> downloadAndPopulateRawData(final Message message) {
        String sourceData = message.getValue("sourceData");
        String url = message.getValue("url");
        
        // init raw data
        final RawData rawData = rawDao.getEmptyInstance();
        rawData.setSourceData(sourceData);
        rawData.setMetaData(message.getMetaData());

        // set source URL
        try {
            rawData.setSourceUrl(new URL(url));
        } catch (final MalformedURLException ex) {
            logger.error("Unable to download from malformed URL {}", url);
            throw new UnrecoverableException("Unable to download data because of malformed url", ex);
        }

        // notice source id
        final Document document = Jsoup.parse(sourceData);
        final String headerText = document.select("div.result_head").first().text();
        String sourceId = null;
        if (headerText != null) {
            final String[] headerTextParts = headerText.split("\\|");
            if (headerTextParts.length > 2) {
                sourceId = headerTextParts[2];
            }
        }
        
        rawData.setPersistentId(getSourceId() + "_" + sha256Hex(sourceId));

        return Arrays.asList(rawData);
    }

    @Override
    public RawDAO<RawData> getRawDataDao() {
        return DAOFactory.getDAOFactory().getRawTenderDAO(getName(), getVersion());
    }

    @Override
    public String getVersion() {
        return VERSION;
    }

    @Override
    protected TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }

    @Override
    protected void postProcess(final RawData raw) {
    }
}
