package eu.datlab.worker.hr.raw;

import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlInput;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import com.gargoylesoftware.htmlunit.html.HtmlSpan;
import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.datlab.worker.raw.BaseDatlabIncrementalPagedSourceHttpCrawler;
import eu.dl.core.UnrecoverableException;
import eu.dl.worker.raw.utils.CrawlerUtils;

import java.io.IOException;
import java.time.LocalDate;
import java.time.Month;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.List;

/**
 * Searches https://eojn.nn.hr/Oglasnik/ for tender details.
 *
 * @author Tomas Mrazek
 */
public final class EOJNTenderCrawler extends BaseDatlabIncrementalPagedSourceHttpCrawler {
    private static final String VERSION = "2";
    private static final String RESULT_SET_URL = PublicationSources.HR_EOJN + "SPIN/application/ipn/PreglediFrm"
            + ".aspx?method=ReducedObjavljeniDokumenti";
    private static final LocalDate DEFAULT_START_DATE = LocalDate.of(2008, Month.JANUARY, 4);
    /**
     * Date format for filter form field value.
     */
    private static final DateTimeFormatter DATE_FILTER_FORMATTER = DateTimeFormatter.ofPattern("d.M.yyyy");
    private static final String NEXT_BUTTON_XPATH =
            "//*[@id='uiView_gridResults']/tbody/tr[@class='up']/td/span[text()=%d]/following-sibling::a";
    /**
     * Web application displays only the first 200 items.
     */
    private static final int RESULTS_COUNT_LIMIT = 200;

    /**
     * @return results page for given date or null if result set contains more then {@code RESULTS_COUNT_LIMIT} records
     */
    @Override
    protected HtmlPage getSearchResultsStartPageForDate(final LocalDate incrementDate) {
        try {
            HtmlPage resultSetPage = filterPageByDate(getWebClient().getPage(RESULT_SET_URL), incrementDate);
            if (getResultsCount(resultSetPage) > RESULTS_COUNT_LIMIT) {
                logger.warn("Results count for date {} is greater then {}", incrementDate, RESULTS_COUNT_LIMIT);
                return null;
            }
            return resultSetPage;
        } catch (IOException e) {
            logger.error("Result set page loading for date {} fails with exception {}.", incrementDate, e);
            throw new UnrecoverableException("Result set page loading fails", e);
        }
    }

    @Override
    public HtmlPage extractDetailsFromPage(final HtmlPage page) {
        final List<HtmlAnchor> detailAnchors = page.getByXPath("//tr[contains(@id, 'uiView_gridResults_Row_')]/td[1]/a");

        for (HtmlAnchor anchor : detailAnchors) {
            createAndPublishMessage(anchor.getHrefAttribute());
        }

        return page;
    }

    @Override
    protected LocalDate getDefaultStartDate() {
        return DEFAULT_START_DATE;
    }

    @Override
    protected String getVersion() {
        return VERSION;
    }

    @Override
    public HtmlPage getNextPage(final HtmlPage actualPage) {
        return CrawlerUtils.clickElement(actualPage, String.format(NEXT_BUTTON_XPATH, getCurrentPageNumber()));
    }

    @Override
    public boolean isPageValid(final HtmlPage page) {
        return page.getElementById("uiView_gridResults") != null && getResultsCount(page) > 0;
    }

    @Override
    protected ChronoUnit getIncrementUnit() {
        return ChronoUnit.DAYS;
    }

    /**
     * Gets the results count from given result page.
     *
     * @param page
     *         page with result set
     *
     * @return count of results. Also returns 0 in case that the element with count was not found on the page.
     */
    private int getResultsCount(final HtmlPage page) {
        final HtmlSpan countSpan = page.getFirstByXPath("//span[@id='uiView_lblRecordCount']/span");
        return (countSpan == null ? 0 : Integer.parseInt(countSpan.asText().replaceAll("[^\\d]+", ""), 10));
    }

    /**
     * Sets the date filter and submit search form.
     *
     * @param page
     *         page with filter form
     * @param date
     *         date for filtering
     *
     * @return page with filtered result list
     * @throws IOException
     *         in case that search form submiting fails
     * @thorws UnrecoverableException
     * in case that page has unexpected structure. Other words script isn't able to set date filter and/or submit.
     */
    private HtmlPage filterPageByDate(final HtmlPage page, final LocalDate date) throws IOException {
        final HtmlInput dateInput = (HtmlInput) page.getElementById("uiFilter_Calendar_DatumObjave_textBox");
        final HtmlAnchor submitButton = page.getFirstByXPath(
                "//tr[@id='uiTraziButton']//a[@class='PreglediTraziButton']");

        if (dateInput == null || submitButton == null) {
            logger.error("Impossible to set filter for date {}");
            throw new UnrecoverableException("Unexpected page structure");
        }
        dateInput.setValueAttribute(date.format(DATE_FILTER_FORMATTER));
        return submitButton.click();
    }
}
