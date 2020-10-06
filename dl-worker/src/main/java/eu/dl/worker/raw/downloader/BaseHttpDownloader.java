package eu.dl.worker.raw.downloader;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

import eu.dl.worker.raw.utils.DownloaderUtils;
import org.apache.commons.io.IOUtils;
import org.jsoup.Connection;

import eu.dl.core.RecoverableException;
import eu.dl.core.UnrecoverableException;
import eu.dl.core.storage.StorageService;
import eu.dl.core.storage.StorageServiceFactory;
import eu.dl.dataaccess.dto.raw.Raw;
import eu.dl.worker.Message;
import eu.dl.worker.utils.NetworkUtils;

/**
 * Simple HTTP downloader encapsulates functionality for downloading source data
 * a creating raw data object.
 * 
 * @param <T>
 *            item to be downloaded
 */
public abstract class BaseHttpDownloader<T extends Raw> extends BaseDownloader<T> {
    private static final String VERSION = "1";

    private final boolean skipExisting;

    /**
     * Default constructor.
     */
    protected BaseHttpDownloader() {
        super();

        // check whether TOR should be started
        if (config.getParam(getName() + ".torEnabled") != null
                && config.getParam(getName() + ".torEnabled").equals("1")) {
            NetworkUtils.enableTorForHttp();
        }

        if (config.getParam(getName() + ".skipExisting") != null) {
            skipExisting = config.getParam(getName() + ".skipExisting").equals("1");
        } else {
            skipExisting = false;
        }
    }

    @Override
    public final List<T> downloadAndPopulateRawData(final Message message) {
        // init raw data
        final T rawData = rawDao.getEmptyInstance();

        // get message parameters
        final String sourceDataUrl = message.getValue("url");
        final String sourceData = message.getValue("sourceData");
        final String sourceBinaryDataUrl = message.getValue("binaryDataUrl");
        final HashMap<String, Object> metaData = message.getMetaData();

        if (sourceDataUrl != null && skipExisting && rawDao.getBySourceUrl(getName(), getVersion(), sourceDataUrl) != null) {
            logger.info("Raw data from {} are already downloaded", sourceDataUrl);
            return Collections.emptyList();
        }

        // download data and populate raw data object
        // valid cases:
        // 1) sourceDataUrl only => save sourceDataUrl, download data from sourceDataUrl and save as source data
        // 2) sourceDataUrl and sourceData => save sourceDataUrl, save the sourceData directly as source data, no
        // need to download anything
        // 3) sourceBinaryDataUrl only => save sourceBinaryDataUrl as source url, download binary data from
        // sourceBinaryDataUrl and save as source binary data

        // 1) sourceDataUrl only
        if (sourceDataUrl != null && sourceData == null && sourceBinaryDataUrl == null) {
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

            // 2) sourceDataUrl and sourceData
        } else if (sourceDataUrl != null && sourceData != null && sourceBinaryDataUrl == null) {
            try {
                rawData.setSourceUrl(new URL(sourceDataUrl));
            } catch (final MalformedURLException ex) {
                logger.error("Unable to set source URL {}", sourceDataUrl);
                throw new UnrecoverableException("Unable to set source URL, because it's malformed", ex);
            }
            rawData.setSourceData(sourceData);
            logger.info("Set data received in message for source URL {}", sourceDataUrl);
            // TODO: should we set the MIME type? it means the request must be executed, so we won't really save any
            // TODO: request (which is why we are sending directly the source data)
            //rawData.setSourceDataMimeType(getUrlResponse(sourceDataUrl).contentType());

            // 3) sourceBinaryDataUrl only
        } else if (sourceDataUrl == null && sourceData == null && sourceBinaryDataUrl != null) {
            InputStream inputStream = null;
            try {
                URL binaryDataUrl = new URL(sourceBinaryDataUrl);
                inputStream = binaryDataUrl.openStream();
                rawData.setSourceBinaryData(IOUtils.toByteArray(inputStream));
                rawData.setSourceUrl(binaryDataUrl);
            } catch (final MalformedURLException ex) {
                logger.error("Unable to download from malformed URL {}", sourceBinaryDataUrl);
                throw new UnrecoverableException("Unable to download file because of malformed url", ex);
            } catch (final IOException ex) {
                logger.error("Unable to download file from url {}", sourceBinaryDataUrl);
                throw new RecoverableException("Unable to download file", ex);
            } finally {
                if (inputStream != null) {
                    try {
                        inputStream.close();
                    } catch (IOException e) {
                        logger.error("Failed to close input stream.");
                    }
                }
            }
            rawData.setSourceDataMimeType(DownloaderUtils.getUrlResponse(sourceBinaryDataUrl).contentType());
            logger.info("Downloaded binary data from {}", sourceBinaryDataUrl);
            // invalid parameters combination
        } else {
            logger.error("Invalid message parameters combination: url: {}, sourceData: {}, binaryDataUrl",
                    sourceDataUrl, sourceData, sourceBinaryDataUrl);
            throw new UnrecoverableException(
                    "Unable to download file because of invalid message parameters combination");
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

            // download additional files
            final List<String> additionalFiles = (ArrayList<String>) metaData.get("additionalFiles");
            if (additionalFiles != null) {
                // save the downloaded staff on disk and store url and uuIds to hash
                final HashMap<String, String> downloadedAdditionalFiles = new HashMap<String, String>();
                final StorageService storageService = StorageServiceFactory.getStorageService();

                // get urls which should be downloaded
                logger.debug("Additional files to be downloaded {}", additionalFiles);

                // download each url
                for (final String additionalFile : additionalFiles) {
                    downloadedAdditionalFiles.put(additionalFile, storageService.save(downloadFile(additionalFile)));
                }

                logger.debug("Additional files downloaded");
                metaData.put("additionalFiles", downloadedAdditionalFiles);
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

    /**
     * Downloads source data from given URL as input stream.
     *
     * @param url
     *         source data URL
     *
     * @return InputStream URL content (source data)
     */
    private InputStream downloadFile(final String url) {
        return new ByteArrayInputStream(DownloaderUtils.getUrlResponse(url).bodyAsBytes());
    }

    @Override
    protected final void postProcess(final T raw) {
    }
}
