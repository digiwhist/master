package eu.datlab.worker.ug.raw;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import eu.datlab.worker.raw.BaseDatlabIncrementalCrawler;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.codetables.OCDSReleaseTag;
import eu.dl.worker.raw.utils.DownloaderUtils;
import org.jsoup.Connection;

import java.io.IOException;
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * Crawler for Uganda.
 */
public final class GPPTenderCrawler extends BaseDatlabIncrementalCrawler {
    private static final String VERSION = "2.0";

    private static final String AFRICA_API_URL = "http://africafoicentre.org/ocds/api/%s?fy=%d-%d&pde_id=%s&type=undefined";

    private static final String GPP_API_URL = "https://gpp.ppda.go.ug/adminapi/public/api/pdes?page=%d";

    private static final LocalDate START_DATE = LocalDate.of(2015, 1, 1);

    private static final Map<OCDSReleaseTag, String> TAGS = new HashMap<>();
    static {
        TAGS.put(OCDSReleaseTag.PLANNING, "plans");
        TAGS.put(OCDSReleaseTag.TENDER, "tenders");
        TAGS.put(OCDSReleaseTag.AWARD, "awards");
    }

    private static final List<String> BUYER_IDS = new ArrayList<>();

    @Override
    protected String getVersion() {
        return VERSION;
    }

    @Override
    protected void initialSetup() {
        // read all pages of buyers
        logger.info("List of buyer ids loading starts");

        int page = 1;
        while (true) {
            Connection.Response response = DownloaderUtils.getUrlResponse(String.format(GPP_API_URL, page), Connection.Method.GET, null);

            if (response.statusCode() >= 300) {
                logger.error("Unable to get page #{} of buyers because of [{}]:{}", page, response.statusCode(), response.statusMessage());
                throw new UnrecoverableException("Unable to get page of buyers");
            }

            JsonNode json;
            try {
                ObjectMapper mapper = new ObjectMapper();
                json = mapper.readTree(response.body());
            } catch(IOException ex) {
                logger.error("Unable to parse JSON from {}", response.url().toString());
                throw new UnrecoverableException("Unable to parse pagination node from JSON", ex);
            }

            Iterator<JsonNode> buyers = json.path("data").path("data").elements();
            while (buyers.hasNext()) {
                JsonNode n = buyers.next();
                BUYER_IDS.add(n.path("id").asText());
            }

            // last page test
            if (page == json.path("data").path("last_page").asInt()) {
                break;
            }

            page += 1;
        }

        logger.info("List of buyer ids loading finished - {} ids found", BUYER_IDS.size());
    }

    @Override
    protected void crawlSourceForDate(final LocalDate date) {
        for (Map.Entry<OCDSReleaseTag, String> n : TAGS.entrySet()) {
            // attempt to get first page of records with the given tag and year
            int year = date.getYear();

            for (String buyerId : BUYER_IDS) {
                String url = String.format(AFRICA_API_URL, n.getValue(), year, year + 1, buyerId);

                HashMap<String, Object> metaData = new HashMap<>();
                metaData.put("year", year);
                metaData.put("tag", n.getKey().name());
                metaData.put("buyerId", buyerId);

                createAndPublishMessage(url, metaData);
            }
        }
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
