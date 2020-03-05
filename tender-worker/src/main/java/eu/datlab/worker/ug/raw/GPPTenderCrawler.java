package eu.datlab.worker.ug.raw;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.datlab.worker.raw.BaseDatlabIncrementalCrawler;
import eu.dl.core.UnrecoverableException;
import eu.dl.worker.raw.utils.DownloaderUtils;
import org.jsoup.Connection;

import java.io.IOException;
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.util.HashMap;

/**
 * Crawler for Uganda.
 */
public final class GPPTenderCrawler extends BaseDatlabIncrementalCrawler {
    private static final String VERSION = "1";

    private static final String SOURCE_URL = PublicationSources.UG_GPP;

    private static final String API_URL = SOURCE_URL + "api/v1/releases";

    private static final LocalDate START_DATE = LocalDate.of(2013, 1, 1);

    /**
     * Number of responses in row with HTTP status 500.
     */
    private static final int ERRORS_COUNT_LIMIT = 10;

    private int errorsCount = 0;

    @Override
    protected String getVersion() {
        return VERSION;
    }

    @Override
    protected void initialSetup() {
    }

    @Override
    protected void crawlSourceForDate(final LocalDate date) {
        for (String t : new String[]{"planning", "tender", "award", "contract", "implementation"}) {
            // attempt to get first page of records with the given tag and year
            int year = date.getYear();

            Connection.Response response = getFirstAvailablePage(t, year, 1);

            HashMap<String, Object> metaData = new HashMap<>();
            metaData.put("year", year);
            metaData.put("tag", t);

            try {
                ObjectMapper mapper = new ObjectMapper();
                JsonNode json = mapper.readTree(response.body());

                int pageCount = json.path("pagination").path("last_page").asInt();
                for (int i = 1; i <= pageCount; i++) {
                    createAndPublishMessage(getPageUrl(t, year, i), metaData);
                }
            } catch(IOException ex) {
                logger.error("Unable to parse pagination node from JSON {}", response.url().toString());
                throw new UnrecoverableException("Unable to parse pagination node from JSON", ex);
            }
        }
    }

    /**
     * @param type
     *      form type
     * @param year
     *      year of records publication
     * @param page
     *      page number
     * @return response
     */
    private Connection.Response getFirstAvailablePage(final String type, final int year, final int page) {
        String url  = getPageUrl(type, year, page);
        Connection.Response response = DownloaderUtils.getUrlResponse(url, Connection.Method.GET, null);

        int status = response.statusCode();

        switch (status) {
            case 200:
                errorsCount = 0;
                return response;
            case 500:
                // For some combination of parameters the api returns HTTP status 500. In such cases, usually the next page returns status
                // 200.
                logger.warn("Unable to get page {} because of {}", url, response.statusMessage());

                if (errorsCount > ERRORS_COUNT_LIMIT) {
                    errorsCount = 0;
                    logger.error("Limit {} of failed requests in row was reached", url,
                        ERRORS_COUNT_LIMIT);
                    throw new UnrecoverableException("Limit of failed requests in row was reached");
                }
                errorsCount += 1;
                return getFirstAvailablePage(type, year, page + 1);
            default:
                logger.error("Unable to get page {} because of HTTP status {}: {}", url, status, response.statusMessage());
                throw new UnrecoverableException("Unable to get page");
        }
    }

    /**
     * @param type
     *      form type
     * @param year
     *      year of records publication
     * @param page
     *      page number
     * @return page url
     */
    private static String getPageUrl(final String type, final int year, final int page) {
        return API_URL + "?tag=" + type + "&fy=" + year + "-" + (year + 1) + "&page=" + page;
    }

    @Override
    protected void finalCleanup() {
    }

    @Override
    protected LocalDate getDefaultStartDate() {
        return START_DATE;
    }

    @Override
    protected ChronoUnit getIncrementUnit() {
        return ChronoUnit.YEARS;
    }
}
