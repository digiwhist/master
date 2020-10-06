package eu.datlab.worker.at.raw;


import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.core.RecoverableException;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.worker.Message;
import eu.dl.worker.raw.BaseCrawler;
import eu.dl.worker.raw.utils.DownloaderUtils;
import org.apache.http.client.utils.URIBuilder;
import org.json.JSONArray;
import org.json.JSONObject;
import org.jsoup.Connection;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.parser.Parser;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.HashMap;

/**
 * Crawler for data.gv.at tenders; uses ckan api.
 *
 * @author Miroslav Brezik
 */
public final class DataGvTenderCrawler extends BaseCrawler {
    private static final String VERSION = "1.0";

    /**
     * Base URL for ckan api calls.
     */
    public static final String BASE_DATA_GV_URL = "https://www.data.gv.at/katalog/api/3/action/package_search";
    /**
     * Page sizes with returned resources.
     */
    public static final int ROWS_INCREMENT = 20;
    /**
     * ckan api tag.
     */
    public static final String TENDER_TAG = "Ausschreibung";

    @Override
    protected String getVersion() {
        return VERSION;
    }

    @Override
    protected void doWork(final Message message) {
        for (int page = 0;; page++) {
            String url = null;
            try {
                URIBuilder uri = new URIBuilder(BASE_DATA_GV_URL);
                uri.addParameter("fq", TENDER_TAG);
                uri.addParameter("rows", String.valueOf(ROWS_INCREMENT));
                uri.addParameter("start", String.valueOf(page * ROWS_INCREMENT));
                url = uri.build().toString();
            } catch (URISyntaxException e) {
                e.printStackTrace();
            }

            JSONObject response = new JSONObject(DownloaderUtils.getResponseBody(url));
            if (!response.has("success") || !response.getBoolean("success")) {
                logger.warn("Response query at url {} has not been successful.", url);
            }

            JSONArray publishers = response.getJSONObject("result").getJSONArray("results");
            if (publishers.isEmpty()) {
                logger.info("No additional publishers acquired. Finishing work.");
                return;
            }

            logger.info("{} publishers acquired. Starting processing...", publishers.length());
            for (int i = 0; i < publishers.length(); i++) {
                JSONArray resources = publishers.getJSONObject(i).getJSONArray("resources");
                for (int j = 0; j < resources.length(); j++) {
                    processResource(resources.getJSONObject(j));
                }
            }
        }

    }

    /**
     * Creates messages for downloader for each tender in a given resource.
     *
     * @param resource JSON resource object
     */
    private void processResource(final JSONObject resource) {
        if (!resource.getString("format").toUpperCase().equals("XML")) {
            return;
        }

        Connection.Response response = null;
        try {
            response = DownloaderUtils.getUrlResponse(resource.getString("url"));
        } catch (RecoverableException e) {
            logger.warn("Resource at {} is not reachable.", resource.getString("url"));
            return;
        } catch (UnrecoverableException e) {
            logger.warn("Resource at {} could not be retreived.", resource.getString("url"));
            return;
        }
        if (response.statusCode() != 200) {
            logger.warn("Skipping resource at {} due to status code {}", resource.getString("url"), response.statusCode());
            return;
        }

        Document document = null;
        if (!response.hasHeader("Accept-Ranges")) {
            String body = DownloaderUtils.getResponseBody(response);
            document = Jsoup.parse(body, "", Parser.xmlParser());
        } else if (response.header("Accept-Ranges").equals("bytes")) {
            try {
                document = Jsoup.parse(response.bodyStream(), null, "", Parser.xmlParser());
            } catch (IOException e) {
                logger.warn("Resource at {} could not be fetched.", resource.getString("url"));
                return;
            }
        } else {
            throw new UnrecoverableException(String.format(
                    "Unknown Accept-Ranges:{} in response header.", response.header("Accept-Ranges")
            ));
        }

        for (Element item : document.select("kdq > item")) {
            final HashMap<String, Object> metaData = new HashMap<>();
//            metaData.put("url", item.selectFirst("url").text());
            metaData.put("lastmod", item.attr("lastmod"));
//            createAndPublishMessage(null, metaData);
            createAndPublishMessage(item.selectFirst("url").text(), metaData);
        }
    }

    @Override
    protected TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }
}
