package eu.digiwhist.worker.no.raw;

import java.io.IOException;
import java.time.LocalDate;
import java.time.Month;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.List;

import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlCheckBoxInput;
import com.gargoylesoftware.htmlunit.html.HtmlElement;
import com.gargoylesoftware.htmlunit.html.HtmlOption;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import com.gargoylesoftware.htmlunit.html.HtmlTextInput;

import eu.digiwhist.worker.raw.BaseDigiwhistIncrementalPagedSourceHttpCrawler;
import eu.dl.core.UnrecoverableException;
import eu.dl.worker.raw.utils.CrawlerUtils;

/**
 * Tender crawler for Doffin in Norway.
 *
 * @author Marek Mikes
 */
public final class DOFFINTenderCrawler extends BaseDigiwhistIncrementalPagedSourceHttpCrawler {
    private static final String VERSION = "1";
    private static final String SEARCH_FORM_URL = "https://www.doffin.no/Notice";

    private static final String INCLUDE_EXPIRED_NOTICES_CHECKBOX_XPATH = "//input[@id='IncludeExpired']";
    private static final String DATE_FROM_INPUT_XPATH = "//input[@id='PublishedFromDate']";
    private static final String DATE_TO_INPUT_XPATH = "//input[@id='PublishedToDate']";
    private static final String SEARCH_BUTTON_XPATH = "//div[@id='filterNoticeSearch']/button";
    private static final String NEXT_BUTTON_XPATH = "//li[not(@class='disabled')]/a/i[@class='icon-forward']";

    private static final LocalDate OLDEST_NOTICE_DATE = LocalDate.of(2003, Month.MAY, 2);
    private static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofPattern("dd/MM/yyyy");

    private static final int MAXIMUM_NUMBER_OF_RESULTS = 1000;

    private static final String NOTICE_XML_PERMALINK_PATTERN = "https://www.doffin.no/Eps"
            + ".Searching/UnsupportedNotice/NoticeXml/%s";

    @Override
    protected HtmlPage getSearchResultsStartPageForDate(final LocalDate incrementDate) {
        try {
            HtmlPage actualPage = getWebClient().getPage(SEARCH_FORM_URL);

            // include expired notices
            final HtmlCheckBoxInput includeExpiredNoticesCheckBoxInput = actualPage.getFirstByXPath(
                    INCLUDE_EXPIRED_NOTICES_CHECKBOX_XPATH);
            includeExpiredNoticesCheckBoxInput.setChecked(true);

            // set publication date
            String localDateString = incrementDate.format(FORMATTER);
            final HtmlTextInput dateFromInput = actualPage.getFirstByXPath(DATE_FROM_INPUT_XPATH);
            dateFromInput.setText(localDateString);
            final HtmlTextInput dateToInput = actualPage.getFirstByXPath(DATE_TO_INPUT_XPATH);
            dateToInput.setText(localDateString);

            final HtmlElement searchButton = actualPage.getFirstByXPath(SEARCH_BUTTON_XPATH);
            actualPage = searchButton.click();

            // number of found entries has to be less or equal than maximum of displayed entries! Maximum of
            // displayed notices is 1000. If there is 100 pages and each page shows 10 notices (default numbers),
            // probably some entries are not displayed.
            final HtmlAnchor goToLastLink = actualPage.getFirstByXPath("//a[child::i[@class='icon-step-forward']]");
            // "Go to last" is not displayed when there are no notices for the date
            if (goToLastLink != null) {
                String totalNumberOfPagesString = goToLastLink.getAttribute("data-current-page");
                // the link can be disabled when all notices are on the first page
                if (!totalNumberOfPagesString.isEmpty()) {
                    int totalNumberOfPages = Integer.parseInt(totalNumberOfPagesString);
                    final HtmlOption numberOfNoticesPerPageOption = actualPage.getFirstByXPath(
                            "//select[@id='PageSize']/option[@selected='selected']");
                    int numberOfNoticesPerPage = Integer.parseInt(numberOfNoticesPerPageOption.getTextContent());
                    if (numberOfNoticesPerPage * totalNumberOfPages == MAXIMUM_NUMBER_OF_RESULTS) {
                        logger.warn("Number of results is probably at least {} (maximum displayed number of " +
                                        "results) for day {}, because there is {} pages and each page contains {} " +
                                        "notices! Next notices will be lost.", MAXIMUM_NUMBER_OF_RESULTS,
                                localDateString,
                                totalNumberOfPagesString, numberOfNoticesPerPageOption.getTextContent());
                    }
                }
            }
            return actualPage;
        } catch (IOException e) {
            logger.error("Getting result for date {} failed with exception {}", incrementDate, e);
            throw new UnrecoverableException("Unable to get result page", e);
        }
    }

    @Override
    public HtmlPage extractDetailsFromPage(final HtmlPage page) {
        @SuppressWarnings("unchecked") final List<HtmlAnchor> noticeLinks = (List<HtmlAnchor>) page.getByXPath(
                "//div[@class='notice-search-item-header']/a[not(child::*)]");
        for (HtmlAnchor noticeLink : noticeLinks) {
            int lastIndexOfSlash = noticeLink.getHrefAttribute().lastIndexOf('/');
            assert lastIndexOfSlash != -1;
            createAndPublishMessage(String.format(NOTICE_XML_PERMALINK_PATTERN,
                    noticeLink.getHrefAttribute().substring(lastIndexOfSlash + 1)));
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
    public String getVersion() {
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
}
