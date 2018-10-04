package eu.datlab.worker.si.raw;

import com.gargoylesoftware.htmlunit.html.HtmlDivision;
import com.gargoylesoftware.htmlunit.html.HtmlElement;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import com.gargoylesoftware.htmlunit.html.HtmlTableRow;
import com.gargoylesoftware.htmlunit.html.HtmlTextInput;
import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.datlab.worker.raw.BaseDatlabIncrementalPagedSourceHttpCrawler;
import eu.dl.core.UnrecoverableException;
import eu.dl.worker.raw.utils.CrawlerUtils;

import java.io.IOException;
import java.time.LocalDate;
import java.time.Month;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.HashMap;
import java.util.List;

/**
 * Tender crawler for E-narocanje in Slovenia.
 *
 * @author Marek Mikes
 */
public final class ENarocanjeTenderCrawler extends BaseDatlabIncrementalPagedSourceHttpCrawler {
    private static final String VERSION = "2";

    private static final String SEARCH_FORM_URL = PublicationSources.SI_ENAROCANJE + "/?podrocje=pregledobjav";
    private static final String DATE_FROM_INPUT_XPATH = "//input[@id='sys_ObjavaDatum_od']";
    private static final String DATE_TO_INPUT_XPATH = "//input[@id='sys_ObjavaDatum_do']";
    private static final String SEARCH_BUTTON_XPATH = "//button[@onclick='doSearch()']";
    private static final String NEXT_BUTTON_XPATH = "//li[@id='seznam_next' and @class='paginate_button next']/a";

    private static final LocalDate OLDEST_NOTICE_DATE = LocalDate.of(2007, Month.JUNE, 27);
    private static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofPattern("dd.MM.yyyy");

    private static final String NOTICE_PERMALINK_PATTERN = PublicationSources.SI_ENAROCANJE + "/Obrazci/?id_obrazec=%s";

    private static final int MAX_WAIT_COUNT = 20;
    private static final long WAIT_LENGTH = 500;

    @Override
    protected HtmlPage getSearchResultsStartPageForDate(final LocalDate incrementDate) {
        try {
            HtmlPage actualPage = getWebClient().getPage(SEARCH_FORM_URL);

            // number of found entries has to be less or equal than maximum of displayed entries! If it is
            // exceeded, span with id 'omejitevTekst' is displayed.
            final HtmlElement noAllNoticesShowedElement = actualPage.getFirstByXPath(
                    "//span[@id='omejitevTekst' and @style='display: inline;']");
            if (noAllNoticesShowedElement != null) {
                logger.warn(
                        "Number of results exceeds maximum displayed number of results for day {}! Next notices " +
                                "will be lost",
                        incrementDate);
            }

            logger.info("Crawler searches for day {}", incrementDate);

            // set publication date
            String localDateString = incrementDate.format(FORMATTER);
            final HtmlTextInput dateFromInput = actualPage.getFirstByXPath(DATE_FROM_INPUT_XPATH);
            dateFromInput.setText(localDateString);
            final HtmlTextInput dateToInput = actualPage.getFirstByXPath(DATE_TO_INPUT_XPATH);
            dateToInput.setText(localDateString);

            final HtmlElement searchButton = actualPage.getFirstByXPath(SEARCH_BUTTON_XPATH);
            actualPage = searchButton.click();

            waitForPageLoad(actualPage);

            return actualPage;
        } catch (IOException e) {
            logger.error("Getting result for date {} failed with exception {}", incrementDate, e);
            throw new UnrecoverableException("Unable to get result page", e);
        }
    }

    @Override
    public HtmlPage extractDetailsFromPage(final HtmlPage page) {
        waitForPageLoad(page);

        final List<HtmlTableRow> noticeTableRows = page.getByXPath("//table[@id='seznam']/tbody/tr");
        for (HtmlTableRow noticeTableRow : noticeTableRows) {
            String noticeId = noticeTableRow.getAttribute("data-id");
            if (noticeId.isEmpty()) {
                // make sure that the page shows no results. It means that the table has just one row and its cell has
                // class attribute "dataTables_empty"
                assert noticeTableRows.size() == 1 && noticeTableRow.getCell(0)
                        .getAttribute("class")
                        .contentEquals("dataTables_empty");
            } else {
                HashMap<String, Object> metaData = new HashMap<>();
                metaData.put("publicationDate",
                    noticeTableRow.getChildElementCount() > 1 ? noticeTableRow.getCell(1).asText() : actualDate);

                createAndPublishMessage(String.format(NOTICE_PERMALINK_PATTERN, noticeId), metaData);
            }
        }

        return page;
    }

    @Override
    public HtmlPage getNextPage(final HtmlPage actualPage) {
        return CrawlerUtils.clickElement(actualPage, NEXT_BUTTON_XPATH);
    }

    @Override
    protected LocalDate getDefaultStartDate() {
        return OLDEST_NOTICE_DATE;
    }

    @Override
    protected String getVersion() {
        return VERSION;
    }

    @Override
    public boolean isPageValid(final HtmlPage page) {
        return true;
    }

    @Override
    protected ChronoUnit getIncrementUnit() {
        return ChronoUnit.DAYS;
    }

    /**
     * Waits some time until the page is not fully loaded.
     *
     * @param page
     *         current list page
     */
    private void waitForPageLoad(final HtmlPage page) {
        //try 'MAX_WAIT_COUNT' times to wait 'WAIT_LENGTH' mili-seconds each for filling the page.
        for (int i = 0; i < MAX_WAIT_COUNT; i++) {
            HtmlDivision visibleWaitingDiv = page.getFirstByXPath(
                    "//div[@id='seznam_processing' and @style='display: block;']");
            if (visibleWaitingDiv == null) {
                logger.info("Page is loaded, crawler continues");
                return;
            }
            logger.info("Page is not loaded, crawler waits {} ms", WAIT_LENGTH);
            synchronized (page) {
                try {
                    page.wait(WAIT_LENGTH);
                } catch (InterruptedException e) {
                    logger.error("Waiting for the page load failed with exception {}", e);
                    throw new UnrecoverableException("Unable to crawl page", e);
                }
            }
        }

        logger.error("Crawler has waited for the page load {} ms with no success", MAX_WAIT_COUNT * WAIT_LENGTH);
        throw new UnrecoverableException("Unable to crawl page");
    }
}
