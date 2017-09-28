package eu.dl.worker.raw.utils;

import eu.dl.core.RecoverableException;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.raw.Raw;
import org.jsoup.Connection;
import org.jsoup.HttpStatusException;
import org.jsoup.Jsoup;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;

import static org.apache.commons.codec.digest.DigestUtils.sha256Hex;

/**
 * Created by michalriha on 21/03/2017.
 */
public final class DownloaderUtils {
    private static final Logger logger = LoggerFactory.getLogger(DownloaderUtils.class);

    private static final boolean VALIDATE_CERTIFICATES = false;
    private static final Integer DOWNLOAD_TIMEOUT = 30000;
    /**
     * Maximum bytes to read from the (uncompressed) connection into the body, before the connection is closed, and the
     * input truncated.
     */
    private static final int MAX_BODY_SIZE = 20000000;

    /**
     * Suppress default constructor for noninstantiability.
     */
    private DownloaderUtils() {
    }

    /**
     * Generates persistent id in the form of PREFIX_md5(sourceUrl).
     *
     * @param raw persistent id is generated for this item
     * @param persistentIdPrefix persistent id prefix
     *
     * @return persistent id or null in case there is no source url set
     */
    public static String generatePersistentId(final Raw raw, final String persistentIdPrefix) {
        String builder = null;
        if (raw.getSourceUrl() != null) {
            builder = raw.getSourceUrl().toString();
            if (raw.getSourceFileName() != null) {
                builder = builder + raw.getSourceFileName();
            }
        }

        if (builder != null) {
            return persistentIdPrefix + "_" + sha256Hex(builder);
        } else {
            return null;
        }
    }

    /**
     * Gets HTTP response body as a string source data.
     *
     * @param response
     *         HTTP response
     *
     * @return response body as string
     */
    public static String getResponseBody(final Connection.Response response) {
        // get the body of the response as a plain string. For HTML page it gets the whole page and for JSON file it
        // gets just the JSON, which is not wrapped by HTML elements (html, head and body).
        // It is not the same as using response.parse().toString() because it does not work for JSON files - it
        // returns the JSON wrapped by HTML elements.
        return response.body();
    }

    /**
     * Gets HTTP response body as a string source data.
     *
     * @param url
     *         source data URL
     *
     * @return String URL content (source data)
     */
    public static String getResponseBody(final String url) {
        return getResponseBody(getUrlResponse(url));
    }

    /**
     * Executes request with given url and returns HTTP response.
     *
     * @param url
     *         source data URL
     *
     * @return HTTP response
     */
    public static Connection.Response getUrlResponse(final String url) {
        try {
            return Jsoup.connect(url)
                    .timeout(DOWNLOAD_TIMEOUT)
                    .validateTLSCertificates(VALIDATE_CERTIFICATES)
                    .ignoreContentType(true)
                    .method(Connection.Method.GET)
                    .maxBodySize(MAX_BODY_SIZE)
                    .execute();
        } catch (final HttpStatusException ex) {
            logger.warn("Http status exception was emitted during getting response for url {}", url);
            throw new RecoverableException("Unable to get response for url", ex);
        } catch (final IOException ex) {
            logger.error("Unable to get response for url {}", url, ex);
            throw new UnrecoverableException("Unable to get response for url", ex);
        }
    }

}
