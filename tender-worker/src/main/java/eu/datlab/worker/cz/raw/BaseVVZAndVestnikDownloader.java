package eu.datlab.worker.cz.raw;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.RawDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.Message;
import eu.dl.worker.raw.downloader.BaseDownloader;
import eu.dl.worker.raw.utils.DownloaderUtils;
import eu.dl.worker.utils.NetworkUtils;
import org.jsoup.Connection;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

/**
 * Base downloader class for Vestnik and VVZ downloader. Allows to skip messages with an url that exists in already downloaded raw data if
 * the configuration property skipExisting is set on 1.
 */
public abstract class BaseVVZAndVestnikDownloader extends BaseDownloader<RawData> {

    private static final String VERSION = "1";

    private final boolean skipExisting;

    /**
     * Default constructor.
     */
    protected BaseVVZAndVestnikDownloader() {
        super();

        // check whether TOR should be started
        if (config.getParam(getName() + ".proxyEnabled") != null
            && config.getParam(getName() + ".proxyEnabled").equals("1")) {
            NetworkUtils.enableProxyForHttp();
        }

        if (config.getParam(getName() + ".skipExisting") != null) {
            skipExisting = config.getParam(getName() + ".skipExisting").equals("1");
        } else {
            skipExisting = false;
        }

    }

    @Override
    protected final boolean skipExisting(final Message message) {
        final String sourceDataUrl = message.getValue("url");
        RawData existing = rawDao.getBySourceUrl(getName(), getVersion(), sourceDataUrl);
        if (existing != null) {
            logger.info("Raw data from {} are already downloaded", sourceDataUrl);
            return true;
        }

        return false;
    }

    @Override
    public final List<RawData> downloadAndPopulateRawData(final Message message) {
        // init raw data
        final RawData rawData = rawDao.getEmptyInstance();

        // get message parameters
        final String sourceDataUrl = message.getValue("url");
        final HashMap<String, Object> metaData = message.getMetaData();

        if (sourceDataUrl != null) {
            RawData existing = rawDao.getBySourceUrl(getName(), getVersion(), sourceDataUrl);
            if (skipExisting && existing != null) {
                logger.info("Raw data from {} are already downloaded", sourceDataUrl);
                return Collections.emptyList();
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
        } else {
            logger.error("Invalid message: url can't be empty");
            throw new UnrecoverableException("Unable to download file because of url is empty");
        }

        // process metadata
        if (metaData != null) {
            logger.debug("Metadata found to be part of the message {}", metaData);

            // download additional urls if necessary
            final List<String> additionalUrls = (ArrayList<String>) metaData.get("additionalUrls");
            if (additionalUrls != null) {
                // store the downloaded staff here
                final HashMap<String, String> downloadedAdditionalUrls = new HashMap<String, String>();

                // get urls which should be downloaded
                logger.debug("Additional urls to be downloaded {}", additionalUrls);

                // download each url
                for (final String additionalUrl : additionalUrls) {
                    downloadedAdditionalUrls.put(additionalUrl, DownloaderUtils.getResponseBody(additionalUrl));
                }

                logger.debug("Additional urls downloaded");
                metaData.put("additionalUrls", downloadedAdditionalUrls);
            }

            rawData.setMetaData(metaData);
        }

        // return results
        return Arrays.asList(rawData);
    }

    @Override
    public final String getVersion() {
        return VERSION;
    }

    @Override
    public final RawDAO getRawDataDao() {
        return DAOFactory.getDAOFactory().getRawTenderDAO(getName(), getVersion());
    }

    @Override
    protected final TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }

    @Override
    protected final void postProcess(final RawData raw) {
    }
}
