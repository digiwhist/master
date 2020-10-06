package eu.datlab.worker.it.raw;

import java.io.IOException;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.X509Certificate;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.jsoup.Jsoup;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;

import eu.dl.dataaccess.dao.DummyTransactionUtils;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.worker.Message;
import eu.dl.worker.raw.BaseCrawler;

import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;

/**
 * Searches http://dati.anticorruzione.it for tender xml url.
 *
 * @author Tomas Mrazek
 */
public final class ANACTenderCrawler extends BaseCrawler {
    private static final String VERSION = "1";

    private static final int DOWNLOAD_TIMEOUT = 30000;


    private static final boolean IGNORE_CONTENT_TYPE = true;
    /**
     * Maximum bytes to read from the (uncompressed) connection into the body, before the connection is closed, and the
     * input truncated.
     */
    private static final int MAX_BODY_SIZE = 20000000;

    private static final String JSON_LIST_URL_TEMPLATE = "http://dati.anticorruzione.it/data/l190-%d.json";
    /**
     * Starting year for tenders crawling.
     */
    private static final int START_CRAWL_YEAR = 2015;
    /**
     * Ending year for tenders crawling.
     */
    private static final int STOP_CRAWL_YEAR = 2016;

    @Override
    protected void doWork(final Message message) {
        // TODO: should use HTMLUnit instead of Jsoup??
        for (int year = START_CRAWL_YEAR; year <= STOP_CRAWL_YEAR; year++) {
            logger.info("Starts tender crawling for year {}.", year);

            final String json = downloadTenderJsonListForYear(year);

            try {
                parseAndPublishTenderUrlsFromJson(json);
            } catch (IOException e) {
                logger.error("Parsing of JSON tender list for year {} fails with exception {}.", year, e);
            }
        }
    }

    @Override
    protected String getVersion() {
        return VERSION;
    }

    /**
     * Parses all urls from json and for each one publishes appropriate message.
     *
     * @param json
     *         json tender list
     *
     * @throws IOException
     *         in case that jason parsing fails
     */
    private void parseAndPublishTenderUrlsFromJson(final String json) throws IOException {
        final JsonFactory factory = new JsonFactory();
        final JsonParser parser = factory.createParser(json);

        while (!parser.isClosed()) {
            final JsonToken jsonToken = parser.nextToken();

            if (JsonToken.FIELD_NAME.equals(jsonToken)) {
                String fieldName = parser.getCurrentName();
                //parses value
                parser.nextToken();

                if ("url".equals(fieldName)) {
                    String url = parser.getValueAsString();
                    if (!hasUrlProtocol(url)) {
                        url = "http://" + url;
                    }
                    logger.info("New tender url {} parsed.", url);
                    createAndPublishMessage(url);
                }
            }
        }
    }

    /**
     * Checks if url string contains protocol (http, ftp etc.).
     *
     * @param url
     *         tested url string
     *
     * @return decision if string contains protocol
     */
    private boolean hasUrlProtocol(final String url) {
        Pattern r = Pattern.compile("(ht|f)tp(s?)://.+");
        Matcher m = r.matcher(url);
        return m.matches();
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
     * Returns json tender list for given year.
     *
     * @param year
     *         year of list
     *
     * @return json tende list
     */
    private String downloadTenderJsonListForYear(final int year) {
        final String url = String.format(JSON_LIST_URL_TEMPLATE, year);

        try {
            return Jsoup.connect(url)
                    .timeout(DOWNLOAD_TIMEOUT)
                    .sslSocketFactory(socketFactory())
                    .ignoreContentType(IGNORE_CONTENT_TYPE)
                    .maxBodySize(MAX_BODY_SIZE)
                    .execute()
                    .body();
        } catch (Exception e) {
            logger.error("JSON tender list downloading from url {} fails.", url);
            return null;
        }
    }

    @Override
    protected TransactionUtils getTransactionUtils() {
        return new DummyTransactionUtils();
    }
}
