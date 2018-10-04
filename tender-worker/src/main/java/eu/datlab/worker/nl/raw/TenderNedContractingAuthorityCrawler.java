package eu.datlab.worker.nl.raw;

import java.net.URL;

import com.gargoylesoftware.htmlunit.html.HtmlPage;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.worker.Message;
import eu.dl.worker.raw.BasePagedSourceHttpCrawler;
import eu.dl.worker.raw.utils.CrawlerUtils;

/**
 * This class is searching
 * https://www.tenderned.nl/tenderned-web/aanbestedendedienst for contracting
 * authority details and save them. Combines functionality of crawler and
 * downloader.
 *
 * @author Tomas Mrazek
 */
public final class TenderNedContractingAuthorityCrawler extends BasePagedSourceHttpCrawler {
    private static final String VERSION = "1";
    private static final String START_PAGE_URL = "https://www.tenderned.nl/tenderned-web/aanbestedendedienst/" +
            "selecteren/aanbestedendedienstlist/cp2/4d790b24e293ae0c3a8a82f5568935a9/van/1716672" +
            "09043be71b52b2a9623cdfbec/islv/a/code/S910/cid/75237";

    private static final String NEXT_BUTTON_XPATH = "//ul[@class='pagination top']/li[@class='next']/a";

    @Override
    public HtmlPage extractDetailsFromPage(final HtmlPage page) {
        final URL url = page.getUrl();
        createAndPublishMessage(url.toString(), page.getWebResponse().getContentAsString());
        return page;
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
    public HtmlPage getNextPage(final HtmlPage actualPage) {
        return CrawlerUtils.clickElement(actualPage, NEXT_BUTTON_XPATH);
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
    protected TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }
}
