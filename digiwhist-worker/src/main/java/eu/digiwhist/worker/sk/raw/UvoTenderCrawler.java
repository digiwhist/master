package eu.digiwhist.worker.sk.raw;

import com.gargoylesoftware.htmlunit.WebClient;
import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import eu.digiwhist.worker.raw.BaseDigiwhistIncrementalCrawler;
import eu.dl.core.UnrecoverableException;
import eu.dl.worker.utils.ThreadUtils;

import java.io.IOException;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.List;

/**
 * Tender crawler for Slovakia.
 */
public final class UvoTenderCrawler extends BaseDigiwhistIncrementalCrawler {
    private static final String VERSION = "2";

    private static final String SOURCE_DOMAIN = "https://www.uvo.gov.sk";
    private static final String SOURCE_URL = SOURCE_DOMAIN + "/dolezite/vestnik-a-registre/vestnik-590.html?date=";

    private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("dd.MM.yyyy");
    private static final LocalDate FIRST_DATE_AVAILABLE = LocalDate.of(2009, 1, 8);

    private static final WebClient webClient = new WebClient();

    private static final int HUMANIZE_TIME = 1000;

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
        // nothing to do
    }

    @Override
    protected void crawlSourceForDate(final LocalDate date) {
        try {
            HtmlPage actualPage = webClient.getPage(SOURCE_URL + date.format(DATE_FORMATTER));
            if (actualPage != null) {
                ThreadUtils.humanize(HUMANIZE_TIME);
                @SuppressWarnings("unchecked") final List<HtmlAnchor> detailPageLinks = (List<HtmlAnchor>) actualPage
                        .getByXPath(
                        "//a[@class='ul-link']");
                for (final HtmlAnchor detailPageLinkRow : detailPageLinks) {
                    createAndPublishMessage(SOURCE_DOMAIN + detailPageLinkRow.getHrefAttribute());
                }
            }
        } catch (final IOException e) {
            logger.error("Crawling failed with exception {}", e);
            throw new UnrecoverableException("Unable to crawl page", e);
        }
    }

    @Override
    protected void finalCleanup() {
        // nothing to do
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
