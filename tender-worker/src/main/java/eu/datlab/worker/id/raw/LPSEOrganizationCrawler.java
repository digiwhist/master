package eu.datlab.worker.id.raw;

import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import com.gargoylesoftware.htmlunit.html.HtmlTable;
import eu.datlab.dataaccess.dao.DAOFactory;
import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.worker.Message;
import eu.dl.worker.raw.BasePagedSourceHttpCrawler;
import eu.dl.worker.raw.utils.CrawlerUtils;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;

/**
 * LPSE organizations crawler in Indonesia.
 *
 * @author Tomas Mrazek
 */
public final class LPSEOrganizationCrawler extends BasePagedSourceHttpCrawler {
    private static final String VERSION = "1.0";

    private static final String SOURCE_DOMAIN = PublicationSources.ID_LPSE;

    private static final String DATATABLE_XPATH = "//table[contains(@class, 'table-list')]";

    @Override
    protected HtmlPage getSearchResultsStartPage(final Message message) {
        try {
            return getWebClient().getPage(SOURCE_DOMAIN);
        } catch (IOException e) {
            logger.error("Crawling failed for {} with exception {}", SOURCE_DOMAIN, e);
            throw new UnrecoverableException("Crawling failed.", e);
        }
    }

    @Override
    public HtmlPage getNextPage(final HtmlPage actualPage) {
        int next = getCurrentPageNumber() + 1;
        return CrawlerUtils.clickElement(actualPage, "//div[@class='pagination']//a[contains(text(), '" + next + "')]");
    }

    @Override
    public HtmlPage extractDetailsFromPage(final HtmlPage page) {
        List<HtmlAnchor> details = page.getByXPath(DATATABLE_XPATH + "/tbody/tr/td/div[@class='title']/a");

        for (HtmlAnchor a : details) {
            HashMap<String, Object> metaData = new HashMap<>();
            metaData.put("name", a.asText());
            createAndPublishMessage(a.getHrefAttribute(), metaData);
        }

        return page;
    }

    @Override
    public boolean isPageValid(final HtmlPage page) {
        HtmlTable table = page.getFirstByXPath(DATATABLE_XPATH);
        return table != null;
    }

    @Override
    protected TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }

    @Override
    protected String getVersion() {
        return VERSION;
    }
}
