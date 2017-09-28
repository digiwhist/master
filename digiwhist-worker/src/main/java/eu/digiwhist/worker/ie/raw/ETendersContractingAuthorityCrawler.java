package eu.digiwhist.worker.ie.raw;

import java.util.List;

import com.gargoylesoftware.htmlunit.html.HtmlPage;
import com.gargoylesoftware.htmlunit.html.HtmlTableRow;

import eu.digiwhist.dataaccess.dao.DAOFactory;
import eu.digiwhist.dataaccess.dto.codetables.PublicationSources;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.worker.raw.BasePagedSourceHttpCrawler;
import eu.dl.worker.raw.utils.CrawlerUtils;

/**
 * Contract authority crawler for Ireland.
 */
public final class ETendersContractingAuthorityCrawler extends BasePagedSourceHttpCrawler {
    private static final String VERSION = "1";
    private static final String SOURCE_DOMAIN = PublicationSources.IE_ETENDERS;
    private static final String START_PAGE_URL = SOURCE_DOMAIN + "/ctm/Company/Search/Index/1";
    private static final String NEXT_BUTTON_XPATH = "//a[@class='pager-action ctm-icon-link']/i[@class='icon-forward']";
    private static final String SEARCH_BUTTON_XPATH = "//a[@id='searchButton']";

    @Override
    public HtmlPage extractDetailsFromPage(final HtmlPage page) {
        // extracts links from first column of the table called table
        final List<HtmlTableRow> detailPageLinks = (List<HtmlTableRow>) page.getByXPath(
                "//table[@class='table']/tbody/tr");
        for (final HtmlTableRow detailPageLinkRow : detailPageLinks) {
            final String detailPageLink = detailPageLinkRow.getCell(0).getFirstElementChild().getAttribute("href");
            createAndPublishMessage(SOURCE_DOMAIN + detailPageLink);
        }

        return page;
    }

    @Override
    protected HtmlPage getSearchResultsStartPage() {
        try {
            logger.debug("Getting the first page to start crawling from.");
            HtmlPage searchPage = getWebClient().getPage(START_PAGE_URL);
            return CrawlerUtils.clickElement(searchPage, SEARCH_BUTTON_XPATH);
        } catch (final Exception e) {
            logger.error("Crawling failed for start page {} with exception {}", START_PAGE_URL, e);
            throw new UnrecoverableException("Crawling failed.", e);
        }
    }

    @Override
    public HtmlPage getNextPage(final HtmlPage actualPage) {
        return CrawlerUtils.clickElement(actualPage, NEXT_BUTTON_XPATH);
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
    protected TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }
}
