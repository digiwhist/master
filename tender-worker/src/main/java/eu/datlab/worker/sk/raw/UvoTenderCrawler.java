package eu.datlab.worker.sk.raw;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import com.gargoylesoftware.htmlunit.WebClient;
import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import eu.datlab.worker.raw.BaseDatlabIncrementalCrawler;
import eu.dl.core.UnrecoverableException;
import eu.dl.worker.raw.utils.DownloaderUtils;
import eu.dl.worker.utils.ThreadUtils;

import java.io.IOException;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.Iterator;
import java.util.List;

/**
 * Tender crawler for Slovakia.
 */
public final class UvoTenderCrawler extends BaseDatlabIncrementalCrawler {
    private static final String VERSION = "2";

    private static final String SOURCE_DOMAIN = "https://www.uvo.gov.sk";
    private static final String SOURCE_URL = SOURCE_DOMAIN + "/vestnik/ajax/calendar?start=%s&end=%s";

    private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd");
    private static final LocalDate FIRST_DATE_AVAILABLE = LocalDate.of(2009, 1, 8);

    private static final int SLEEP_MS = 2 * 60 * 1000;

    private static final WebClient webClient = new WebClient();

    /**
     * Default constructor.
     */
    public UvoTenderCrawler() {
        super();
        webClient.getOptions().setUseInsecureSSL(true);
        webClient.getOptions().setThrowExceptionOnScriptError(false);
        webClient.getOptions().setJavaScriptEnabled(false);
    }

    @Override
    protected void initialSetup() {
    }

    @Override
    protected void crawlSourceForDate(final LocalDate date) {
        try {
            // request on one day (from date to date + 1d) doesn't work.
            String url = String.format(SOURCE_URL,
                date.minusDays(2).format(DATE_FORMATTER),
                date.plusDays(2).format(DATE_FORMATTER));

            String response = DownloaderUtils.getResponseBody(url);
            if (!response.startsWith("[")) {
                ThreadUtils.sleep(SLEEP_MS);
                response = DownloaderUtils.getResponseBody(url);
            }

            ObjectMapper mapper = new ObjectMapper();
            JsonNode json = mapper.readTree(response);

            Iterator<JsonNode> rows = json.elements();

            String now = date.format(DATE_FORMATTER);
            while (rows.hasNext()) {
                JsonNode r = rows.next();
                String current = r.get("start").textValue();
                if (current.equals(now)) {
                    HtmlPage actualPage = webClient.getPage(SOURCE_DOMAIN + r.get("url").textValue());
                    if (actualPage != null) {
                        final List<HtmlAnchor> detailPageLinks = actualPage.getByXPath("//a[@class='ul-link']");
                        for (final HtmlAnchor detailPageLinkRow : detailPageLinks) {
                            createAndPublishMessage(SOURCE_DOMAIN + detailPageLinkRow.getHrefAttribute());
                        }
                    }
                } else if (current.compareTo(now) > 0) {
                    break;
                }
            }
        } catch (final IOException e) {
            logger.error("Crawling failed with exception {}", e);
            throw new UnrecoverableException("Unable to crawl page", e);
        }
    }

    @Override
    protected void finalCleanup() {
    }

    @Override
    protected LocalDate getDefaultStartDate() {
        return FIRST_DATE_AVAILABLE;
    }

    @Override
    protected String getVersion() {
        return VERSION;
    }

    @Override
    protected ChronoUnit getIncrementUnit() {
        return ChronoUnit.DAYS;
    }
}
