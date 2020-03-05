package eu.datlab.worker.py.raw;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import eu.datlab.dataaccess.dao.DAOFactory;
import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.dl.core.RecoverableException;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.RawDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.Message;
import eu.dl.worker.raw.downloader.BaseDownloader;
import eu.dl.worker.raw.utils.DownloaderUtils;
import eu.dl.worker.utils.ThreadUtils;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.jsoup.Connection;

/**
 * Tender downloader for Paraguay.
 *
 * @author Tomas Mrazek
 */
public final class DNCPTenderDownloader extends BaseDownloader<RawData> {

    private static final String REQUEST_TOKEN =
        "ZTEwYWE2ZjYtMDZmZC00ODZjLTgzZTMtMjkxNmVlYWE4MDlhOjg2NmZmMzUxLTBhOWEtNDdkYi1hODdlLTZkZTY0OTJiMTFhNQ==";

    private String authToken;

    private static final String SOURCE_DOMAIN = PublicationSources.PY_DNCP;

    private static final int API_PORT = 443;

    private static final String BASE_API_URL = SOURCE_DOMAIN + ":" + API_PORT + "/datos/api/v2/";

    private static final String VERSION = "1.0";

    @Override
    public List<RawData> downloadAndPopulateRawData(final Message message) {
        // init raw data
        final RawData rawData = rawDao.getEmptyInstance();

        // get message parameters
        final String sourceDataUrl = message.getValue("url");
        final HashMap<String, Object> metaData = message.getMetaData();

        // download data and populate raw data object
        try {
            rawData.setSourceUrl(new URL(sourceDataUrl));
        } catch (final MalformedURLException ex) {
            logger.error("Unable to download from malformed URL {}", sourceDataUrl);
            throw new UnrecoverableException("Unable to download data because of malformed url", ex);
        }

        if (authToken == null) {
            authToken = getAuthToken();
            logger.debug("Authorization token {} retrieved", authToken);
        }

        final Connection.Response response = getApiResponse(sourceDataUrl, authToken);
        if (response == null) {
            return Collections.emptyList();
        }

        // download CSV bidders list
        if (metaData != null && metaData.containsKey("bidders")) {
            String biddersUrl = (String) metaData.get("bidders");
            if (biddersUrl != null) {
                Connection.Response biddersCSV = DownloaderUtils.getUrlResponse(biddersUrl, Connection.Method.GET, null);
                switch (biddersCSV.statusCode()) {
                    case 200:
                        metaData.put("bidders", Collections.singletonMap(biddersUrl, DownloaderUtils.getResponseBody(biddersCSV)));
                        logger.debug("Bidders CSV downloaded from {}", biddersUrl);
                        break;
                    case 404:
                        logger.debug("Bidders CSV {} not found", biddersUrl);
                        metaData.remove("bidders");
                        break;
                    default:
                        // unexpected response status
                        logger.error("Unable to download bidders CSV because of {}", response.statusMessage());
                        throw new UnrecoverableException("Unable to download bidders CSV");
                }
            }
        }

        logger.info("Downloaded data from {}", sourceDataUrl);
        rawData.setSourceData(DownloaderUtils.getResponseBody(response));
        rawData.setSourceDataMimeType(response.contentType());
        rawData.setMetaData(metaData);

        return Arrays.asList(rawData);
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

    /**
     * @return authorization token
     */
    private String getAuthToken() {
        Connection.Response response = DownloaderUtils.getUrlResponse(BASE_API_URL + "oauth/token", Connection.Method.POST,
            Collections.singletonMap("Authorization", "Basic " + REQUEST_TOKEN));

        try {
            ObjectMapper mapper = new ObjectMapper();
            JsonNode json = mapper.readTree(response.body());
            return json.path("access_token").asText(null);
        } catch(IOException ex) {
            logger.error("Unable to parse access_token");
            throw new UnrecoverableException("Unable to parse access_token", ex);
        }
    }

    /**
     * @param token
     *      authorization token to be invalidated
     */
    private void invalidateAuthToken(final String token) {
        Map<String, String> headers = new HashMap<>();
        headers.put("Authorization", "Basic " + REQUEST_TOKEN);
        headers.put("access_token", token);

        Connection.Response response = DownloaderUtils.getUrlResponse(BASE_API_URL + "oauth/invalidate_token", Connection.Method.POST,
            headers);
    }

    /**
     * @param url
     *      requested API url
     * @param token
     *      authorization token
     * @return API response or NULL
     */
    private Connection.Response getApiResponse(final String url, final String token) {
        Connection.Response response = DownloaderUtils.getUrlResponse(url, Connection.Method.GET,
            Collections.singletonMap("Authorization", "Bearer " + token));

        switch (response.statusCode()) {
            case 200:
                return response;
            case 401:
                logger.debug("Authorization token {} expired", authToken);
                invalidateAuthToken(authToken);
                logger.debug("Authorization token {} invalidated", authToken);
                authToken = getAuthToken();
                logger.debug("Authorization token {} retrieved", authToken);

                return getApiResponse(url, authToken);
            case 404:
                logger.warn("Record not found on {}", url);
                return null;
            case 500:
                logger.warn("Internal Server Error for {}", url);
                throw new RecoverableException("Internal Server Error");
            case 429:
                logger.warn("Limit of 5000 requests in 15 minutes was reached");
                ThreadUtils.sleep(30000);
                return getApiResponse(url, authToken);
            case 502:
            default:
                System.out.println(response.body());
                logger.error("Unable to get API response from {} with token {}", url, token);
                throw new UnrecoverableException("Unable to get API response");
        }
    }

    @Override
    protected void postProcess(final RawData raw) {
    }
}
