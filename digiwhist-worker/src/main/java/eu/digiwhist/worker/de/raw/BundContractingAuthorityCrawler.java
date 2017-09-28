package eu.digiwhist.worker.de.raw;

import java.util.List;

import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlPage;

import eu.digiwhist.dataaccess.dao.DAOFactory;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.worker.raw.BasePagedSourceHttpCrawler;
import eu.dl.worker.raw.utils.CrawlerUtils;

/**
 * This class is searching http://www.bund.de for contracting authority
 * detail url.
 *
 * @author Tomas Mrazek
 */
public final class BundContractingAuthorityCrawler extends BasePagedSourceHttpCrawler {
    private static final String VERSION = "1";
    private static final String SOURCE_DOMAIN = "http://www.bund.de/";
    private static final String START_PAGE_URL = SOURCE_DOMAIN + "Content/DE/Behoerden/Suche/Formular"
            + ".html?nn=4641496&resultsPerPage=100";
    private static final String NEXT_BUTTON_XPATH = "//div[@class='pager item-m']//li[@class='next']/a";

    @Override
    public HtmlPage extractDetailsFromPage(final HtmlPage page) {
        final List<HtmlAnchor> nodes = (List<HtmlAnchor>) page.getByXPath("//ul[@class='result-list']/li/a");

        for (final HtmlAnchor detail : nodes) {
            createAndPublishMessage(SOURCE_DOMAIN + detail.getHrefAttribute());
        }
        return page;
    }

    @Override
    protected HtmlPage getSearchResultsStartPage() {
        try {
            logger.debug("Getting the first page to start crawling from.");
            return getWebClient().getPage(START_PAGE_URL);
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
