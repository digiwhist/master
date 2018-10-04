package eu.datlab.worker.hu.raw;

import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlElement;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import com.gargoylesoftware.htmlunit.html.HtmlTextInput;
import eu.datlab.worker.raw.BaseDatlabIncrementalPagedSourceHttpCrawler;
import eu.dl.core.UnrecoverableException;
import eu.dl.worker.raw.utils.CrawlerUtils;
import org.apache.commons.lang3.StringUtils;

import java.io.IOException;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.logging.Level;

/**
 * Tender crawler for Hungary.
 *
 * @author Michal Riha
 */
public final class KHTenderCrawler extends BaseDatlabIncrementalPagedSourceHttpCrawler {
    private static final String VERSION = "1";

    private static final String SOURCE_DOMAIN = "http://kozbeszerzes.hu/";
    private static final String SEARCH_FORM_URL = SOURCE_DOMAIN + "adatbazis/keres/hirdetmeny/";

    private static final String DATE_FROM_INPUT_XPATH = "//input[@id='id_dtr_mt_megjelenesiDatum_from']";
    private static final String DATE_TO_INPUT_XPATH = "//input[@id='id_dtr_mt_megjelenesiDatum_to']";
    private static final String SEARCH_BUTTON_XPATH = "//form[@id='searchForm']//button[@type='submit']";
    private static final String NEXT_BUTTON_XPATH = "//span[@class='endless_page_current']/following-sibling::a";

    private static final LocalDate OLDEST_NOTICE_DATE = LocalDate.of(2013, 1, 1);
    private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd");

    private String currentDateString = null;

    /**
     * Pages have some javascript errors, they work, but generate a lot of warnings to logs, line in constructor
     * hides them.
     */
    public KHTenderCrawler() {
        super();
        java.util.logging.Logger.getLogger("com.gargoylesoftware").setLevel(Level.OFF);
//        getWebClient().getOptions().setJavaScriptEnabled(true);
    }

    @Override
    protected HtmlPage getSearchResultsStartPageForDate(final LocalDate incrementDate) {
        HtmlPage actualPage;
        try {
            actualPage = getWebClient().getPage(SEARCH_FORM_URL);
        } catch (IOException e) {
            logger.error("Crawling failed cannot load search page", getCurrentPageUrl(), e);
            throw new UnrecoverableException("Crawling failed cannot load search page", e);
        }

        // set publication date
        currentDateString = incrementDate.format(DATE_FORMATTER);
        return filterByDate(actualPage);
    }

    @Override
    public HtmlPage extractDetailsFromPage(final HtmlPage page) {
        final List<HtmlAnchor> nodes = page.getByXPath("//table[@class='results results__hirdetmeny results_with-link']//tr/td[1]/a");
        for (final HtmlAnchor detail : nodes) {
            createAndPublishMessage(SOURCE_DOMAIN + StringUtils.stripStart(detail.getHrefAttribute(), "/"));
        }

        return page;
    }

    @Override
    public boolean isPageValid(final HtmlPage page) {
        return true;
    }

    @Override
    protected LocalDate getDefaultStartDate() {
        return OLDEST_NOTICE_DATE;
    }

    @Override
    public HtmlPage getNextPage(final HtmlPage actualPage) {
        return filterByDate(CrawlerUtils.clickElement(actualPage, NEXT_BUTTON_XPATH));
    }

    @Override
    protected String getVersion() {
        return VERSION;
    }

    @Override
    protected ChronoUnit getIncrementUnit() {
        return ChronoUnit.DAYS;
    }

    /**
     * This crawler needs to filter by date on search and than on every next page click.
     *
     * @param actualPage actualPage
     * @return HtmlPage or null
     */
    private HtmlPage filterByDate(final HtmlPage actualPage) {
        if (actualPage == null) {
            return null;
        }

        final HtmlTextInput dateFromInput = actualPage.getFirstByXPath(DATE_FROM_INPUT_XPATH);
        dateFromInput.setText(currentDateString);
        final HtmlTextInput dateToInput = actualPage.getFirstByXPath(DATE_TO_INPUT_XPATH);
        dateToInput.setText(currentDateString);

        final HtmlElement searchButton = actualPage.getFirstByXPath(SEARCH_BUTTON_XPATH);

        try {
            return searchButton.click();
        } catch (IOException e) {
            logger.error("Crawling failed for page with url {} with exception {}", getCurrentPageUrl(), e);
            throw new UnrecoverableException("Unable to crawl page", e);
        }
    }
}
