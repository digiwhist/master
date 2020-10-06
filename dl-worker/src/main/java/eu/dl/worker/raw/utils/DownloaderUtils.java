package eu.dl.worker.raw.utils;

import eu.dl.core.RecoverableException;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.raw.Raw;
import org.jsoup.Connection;
import org.jsoup.HttpStatusException;
import org.jsoup.Jsoup;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;
import java.io.IOException;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.X509Certificate;
import java.util.Collections;
import java.util.Map;

import static org.apache.commons.codec.digest.DigestUtils.sha256Hex;

/**
 * Created by michalriha on 21/03/2017.
 */
public final class DownloaderUtils {
    private static final Logger logger = LoggerFactory.getLogger(DownloaderUtils.class);

    private static final Integer DOWNLOAD_TIMEOUT = 500000;
    /**
     * Maximum bytes to read from the (uncompressed) connection into the body, before the connection is closed, and the
     * input truncated.
     */
    private static final int MAX_BODY_SIZE = 250000000;

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
     * Executes GET request with given url and returns HTTP response.
     *
     * @param url
     *         source data URL
     *
     * @return HTTP response
     */
    public static Connection.Response getUrlResponse(final String url) {
        return getUrlResponse(url, DOWNLOAD_TIMEOUT);
    }


    /**
     * Checks SSL certificates.
     * @return socketFactory
     */
    private static SSLSocketFactory socketFactory() {
        TrustManager[] trustAllCerts = new TrustManager[]{new X509TrustManager() {
            public java.security.cert.X509Certificate[] getAcceptedIssuers() {
                return null;
            }

            public void checkClientTrusted(final X509Certificate[] certs, final String authType) {
            }

            public void checkServerTrusted(final X509Certificate[] certs, final String authType) {
            }
        }};

        try {
            SSLContext sslContext = SSLContext.getInstance("TLS");
            sslContext.init(null, trustAllCerts, new java.security.SecureRandom());
            return sslContext.getSocketFactory();
        } catch (NoSuchAlgorithmException | KeyManagementException e) {
            throw new RuntimeException("Failed to create a SSL socket factory", e);
        }
    }

    /**
     * Executes GET request with given url and returns HTTP response.
     *
     * @param url
     *         source data URL
     * @param downloadTimeout
     *         download timeout
     *
     * @return HTTP response
     */
    public static Connection.Response getUrlResponse(final String url, final Integer downloadTimeout) {
        try {
            return Jsoup.connect(url)
                    .timeout(downloadTimeout)
                    .sslSocketFactory(socketFactory())
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

    /**
     * Executes HTTP request with given url and returns response.
     *
     * @param url
     *      requested URL
     * @param method
     *      request method, if NULL the GET method is used
     * @param headers
     *      request headers
     * @param data
     *      request payload
     * @return HTTP response
     */
    public static Connection.Response getUrlResponse(final String url, final Connection.Method method, final Map<String, String> headers,
                                                     final Map<String, String> data) {
        try {
            return Jsoup.connect(url)
                .timeout(DOWNLOAD_TIMEOUT)
                .sslSocketFactory(socketFactory())
                .ignoreContentType(true)
                .method(method == null ? Connection.Method.GET : method)
                .maxBodySize(MAX_BODY_SIZE)
                .headers(headers == null ? Collections.emptyMap() : headers)
                .ignoreHttpErrors(true)
                .data(data == null ? Collections.emptyMap() : data)
                .execute();        
        } catch (final IOException ex) {
            logger.error("Unable to get response for url {}", url, ex);
            throw new UnrecoverableException("Unable to get response for url", ex);
        }
    }

    /**
     * Executes HTTP request with given url and returns response.
     *
     * @param url
     *      requested URL
     * @param method
     *      request method, if NULL the GET method is used
     * @param headers
     *      request headers
     * @return HTTP response
     */
    public static Connection.Response getUrlResponse(final String url, final Connection.Method method, final Map<String, String> headers) {
        return getUrlResponse(url, method, headers, null);
    }
}
