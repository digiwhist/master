package eu.datlab.worker.sk.raw;

import com.gargoylesoftware.htmlunit.html.HtmlPage;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.worker.Message;
import eu.dl.worker.raw.BasePagedSourceHttpCrawler;
import eu.dl.worker.raw.utils.CrawlerUtils;

/**
 * Contracting authority crawler for Slovakia.
 *
 * @author Michal Riha
 */
public final class UvoContractingAuthorityCrawler extends BasePagedSourceHttpCrawler {
    private static final String VERSION = "1";
    private static final String SOURCE_DOMAIN = "https://www.uvo.gov.sk";
    private static final String START_PAGE_URL = SOURCE_DOMAIN + "/profily";
    private static final String NEXT_BUTTON_XPATH = "//a[contains(text(), 'Ďalšia ')]";

    /**
     * Default constructor.
     */
    public UvoContractingAuthorityCrawler() {
        super();
        // JavaScript not needed and disabling drastically speeds up the process
        getWebClient().getOptions().setJavaScriptEnabled(false);
    }

    @Override
    protected HtmlPage getSearchResultsStartPage(final Message message) {
        try {
            logger.debug("Getting the first page to start crawling from.");
            return getWebClient().getPage(START_PAGE_URL);
        } catch (final Exception e) {
            logger.error("Crawling failed for start page {} with exception {}", START_PAGE_URL, e);
            throw new UnrecoverableException("Crawling failed.", e);
        }
    }

    @Override
    public HtmlPage extractDetailsFromPage(final HtmlPage page) {
        createAndPublishMessage(page.getUrl().toString());
        return page;
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
