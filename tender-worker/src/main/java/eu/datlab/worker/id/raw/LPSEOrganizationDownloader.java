package eu.datlab.worker.id.raw;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.RawDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.Message;
import eu.dl.worker.raw.downloader.BaseDownloader;
import eu.dl.worker.raw.utils.DownloaderUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.Connection;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

/**
 * LPSE organizations downloader in Indonesia.
 *
 * @author Tomas Mrazek
 */
public final class LPSEOrganizationDownloader extends BaseDownloader<RawData> {

    /**
     * Worker version.
     */
    public static final String VERSION = "1.0";

    /**
     * Raw DAO initialization.
     */
    public LPSEOrganizationDownloader() {
        super();
    }

    @Override
    protected boolean skipExisting(final Message message) {
        return false;
    }

    @Override
    public List<RawData> downloadAndPopulateRawData(final Message message) {

        // init raw data
        final RawData rawData = rawDao.getEmptyInstance();

        // get message parameters
        final String sourceDataUrl = message.getValue("url");
        final HashMap<String, Object> metaData = message.getMetaData();

        if (sourceDataUrl == null) {
            logger.error("Unable to download subject's detail because of url is empty. message: {}", message.toJson());
            throw new UnrecoverableException("Unable to download subject's detail because of url is empty");
        }

        try {
            rawData.setSourceUrl(new URL(sourceDataUrl));
        } catch (final MalformedURLException ex) {
            logger.error("Unable to download from malformed URL {}", sourceDataUrl);
            throw new UnrecoverableException("Unable to download data because of malformed url", ex);
        }


        final Connection.Response response = DownloaderUtils.getUrlResponse(sourceDataUrl);
        logger.info("Downloaded data from {}", sourceDataUrl);

        rawData.setSourceData(DownloaderUtils.getResponseBody(response));
        rawData.setSourceDataMimeType(response.contentType());

        Document doc = Jsoup.parse(rawData.getSourceData());
        String url = JsoupUtils.selectText("strong:containsOwn(URL) + p > a", doc);

        if (url == null || url.isEmpty()) {
            logger.error("Unable to parse subject's bulletin url from {}", sourceDataUrl);
            throw new UnrecoverableException("Unable to parse subject's bulletin url");
        }

        metaData.put("url", url);
        rawData.setMetaData(metaData);

        // return results
        return Arrays.asList(rawData);
    }

    @Override
    protected void postProcess(final RawData raw) {
    }

    @Override
    public String getVersion() {
        return VERSION;
    }

    @Override
    public RawDAO getRawDataDao() {
        return DAOFactory.getDAOFactory().getRawTenderDAO(getName(), getVersion());
    }

    @Override
    protected TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }
}
