package eu.datlab.worker.ug.raw;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.RawDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.Message;
import eu.dl.worker.raw.downloader.BaseDownloader;
import eu.dl.worker.raw.utils.DownloaderUtils;
import org.jsoup.Connection;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

/**
 * Downloader for Uganda.
 */
public final class GPPTenderDownloader extends BaseDownloader<RawData> {
    private static final String VERSION = "1.0";

    @Override
    protected boolean skipExisting(final Message message) {
        final String sourceDataUrl = message.getValue("url");
        RawData existing = rawDao.getBySourceUrl(getName(), getVersion(), sourceDataUrl);
        if (existing != null) {
            logger.info("Raw data from {} are already downloaded", sourceDataUrl);
            return true;
        }

        return false;
    }

    @Override
    public List<RawData> downloadAndPopulateRawData(final Message message) {
        final RawData rawData = rawDao.getEmptyInstance();

        // get message parameters
        final String sourceDataUrl = message.getValue("url");
        final HashMap<String, Object> metaData = message.getMetaData();

        try {
            rawData.setSourceUrl(new URL(sourceDataUrl));
        } catch (final MalformedURLException ex) {
            logger.error("Unable to download from malformed URL {}", sourceDataUrl);
            throw new UnrecoverableException("Unable to download data because of malformed url", ex);
        }

        final Connection.Response response = DownloaderUtils.getUrlResponse(sourceDataUrl, Connection.Method.GET, null);
        if (response.statusCode() == 200) {
            logger.info("Downloaded data from {}", sourceDataUrl);
            rawData.setSourceData(DownloaderUtils.getResponseBody(response));
            rawData.setSourceDataMimeType(response.contentType());
            rawData.setMetaData(metaData);

            return Collections.singletonList(rawData);
        } else {
            logger.error("Unable to download data from {} because of the page is broken.", sourceDataUrl);
            throw new UnrecoverableException("Unable to download data from broken page");
        }
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

    @Override
    protected void postProcess(final RawData raw) {
    }
}
