package eu.datlab.worker.pl.raw;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.core.RecoverableException;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.RawDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.raw.Raw;
import eu.dl.worker.Message;
import eu.dl.worker.raw.downloader.BaseDownloader;
import eu.dl.worker.raw.utils.DownloaderUtils;
import org.jsoup.Connection;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Collections;
import java.util.List;

/**
 * UZP Tender JSON downloader.
 *
 * @param <T>
 *            item to be downloaded
 *
 */
public final class UZPTenderJsonDownloader<T extends Raw> extends BaseDownloader<T> {
    private static final String VERSION = "1";

    private static final int MAX_ATTEMPTS = 20;
    private static final Integer DOWNLOAD_TIMEOUT = 600000;

    @Override
    public List<T> downloadAndPopulateRawData(final Message message) {
        final T rawData = rawDao.getEmptyInstance();

        final String sourceDataUrl = message.getValue("url");
        rawData.setMetaData(message.getMetaData());

        if (sourceDataUrl != null) {
            try {
                rawData.setSourceUrl(new URL(sourceDataUrl));
            } catch (final MalformedURLException ex) {
                logger.error("Malformed URL {}", sourceDataUrl);
                throw new UnrecoverableException("Unable to download data because of malformed url", ex);
            }

            int attempts = 0;
            while (attempts < MAX_ATTEMPTS) {
                try {
                    final Connection.Response response = DownloaderUtils.getUrlResponse(sourceDataUrl, DOWNLOAD_TIMEOUT);
                    logger.info("Downloaded data from {}", sourceDataUrl);
                    rawData.setSourceData(DownloaderUtils.getResponseBody(response));
                    rawData.setSourceDataMimeType(response.contentType());
                    break;
                } catch (RecoverableException e) {
                    attempts++;
                    logger.warn("Server halted request, trying again for {} time", attempts);
                } catch (UnrecoverableException e) {
                    attempts++;
                    logger.warn("Download timed out, trying again for {} time", attempts);
                }
            }

            if (attempts >= MAX_ATTEMPTS) {
                logger.error("Not able to download from url: {}", sourceDataUrl);
                throw new UnrecoverableException("Not able to download from url: " + sourceDataUrl);
            }
        } else {
            logger.error("Invalid url, url NULL");
            throw new UnrecoverableException("Invalid url, url NULL");
        }

        // return result
        return Collections.singletonList(rawData);
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
    protected void postProcess(final T raw) {
    }
}
