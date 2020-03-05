package eu.datlab.worker.in.raw;

import com.gargoylesoftware.htmlunit.html.HtmlPage;
import com.gargoylesoftware.htmlunit.html.HtmlSelect;
import eu.datlab.dataaccess.dao.DAOFactory;
import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.datlab.worker.raw.BaseDatlabIncrementalPagedSourceHttpCrawler;
import eu.dl.dataaccess.dao.CrawlerAuditDAO;
import eu.dl.dataaccess.dto.raw.CrawlerAuditRecord;
import eu.dl.worker.Message;
import eu.dl.worker.raw.utils.CrawlerUtils;
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.util.List;

/**
 * Tender crawler for India searches tab Results.
 *
 * @author Tomas Mrazek
 */
public final class CPPPResultsTenderCrawler extends BaseDatlabIncrementalPagedSourceHttpCrawler {
    private static final String VERSION = "1.0";

    private static final String SOURCE_DOMAIN = PublicationSources.IN_CPPP;

    private static final String SEARCH_FORM_URL = SOURCE_DOMAIN + "cppp/resultoftenders";

    private static final LocalDate DEFAULT_START_DATE = LocalDate.of(2013, 1, 1);

    private final CrawlerAuditDAO<CrawlerAuditRecord> auditDAO = DAOFactory.getDAOFactory().getCrawlerAuditDAO(getName(), getVersion());

    @Override
    protected HtmlPage getSearchResultsStartPageForDate(final LocalDate incrementDate) {
        // search form is protected by CAPTCHA
        HtmlPage actualPage = CPPPCrawlerUtils.getCAPTCHAProtectedPage(SEARCH_FORM_URL);

        HtmlSelect yearSelect  = actualPage.getFirstByXPath("//select[@name='year']");

        int year = incrementDate.getYear();

        actualPage = yearSelect.setSelectedAttribute(String.valueOf(year), true);

        actualPage = CrawlerUtils.clickElement(actualPage, "//input[@type='submit' and @value='Search']");

        CrawlerAuditRecord auditRecord = auditDAO.getByNameAndVersion();

        Integer lastPage = auditRecord.getLastCrawledPageNumber();
        LocalDate lastCrawledDate = auditRecord.getLastCrawledDate();
        // use last page only if the currently crawled year is following after last crawled year
        // Note:
        // - last crawled date is updated after successful crawling of all pages given year
        // - last crawled page is updated each time when the page is crawled
        if (lastPage != null && (lastCrawledDate == null || year == (lastCrawledDate.getYear() + 1))) {
            logger.info("Last page number {} found in crawler audit record, start at next page", lastPage);
            setCurrentPageNumber(lastPage + 1);
            // To URL of first page add page parameter with required page number. Unable to use page button, button of required page may not
            // be on the first page.
            actualPage = CPPPCrawlerUtils.getNthPage(actualPage, getCurrentPageNumber());
        }

        return actualPage;
    }

    @Override
    public HtmlPage getNextPage(final HtmlPage actualPage) {
        HtmlPage nextPage = CPPPCrawlerUtils.getNthPage(actualPage, getCurrentPageNumber() + 1);
        return CPPPCrawlerUtils.getRecords(nextPage).size() > 0 ? nextPage : null;
    }

    @Override
    public HtmlPage extractDetailsFromPage(final HtmlPage page) {
        List<Message> messages = CPPPCrawlerUtils.extractDetailsFromPage(page, 2, 3);

        messages.forEach(m -> publishMessage(m));

        auditDAO.updateLastCrawledPageNumberForCrawler(getCurrentPageNumber());

        return page;
    }

    @Override
    public boolean isPageValid(final HtmlPage page) {
        return true;
    }

    @Override
    protected String getVersion() {
        return VERSION;
    }

    @Override
    protected LocalDate getDefaultStartDate() {
        return DEFAULT_START_DATE;
    }

    @Override
    protected ChronoUnit getIncrementUnit() {
        return ChronoUnit.YEARS;
    }
}
