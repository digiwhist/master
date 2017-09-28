package eu.digiwhist.worker.mt.raw;

import java.io.IOException;
import java.util.List;

import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlPage;

import eu.digiwhist.dataaccess.dao.DAOFactory;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.worker.Message;
import eu.dl.worker.raw.BaseHttpCrawler;

/**
 * Contracting authority crawler for Malta.
 *
 * @author Michal Riha
 */
public final class GovContractingAuthorityCrawler extends BaseHttpCrawler {
    private static final String VERSION = "1";
    private static final String DOMAIN_URL = "https://www.gov.mt";

    private static final String FIRST_PAGE_URL = DOMAIN_URL +
            "/en/Government/Government%20Gazette/tendersnew/Pages/Tenders-and-Contracts.aspx";

    @Override
    protected void doWork(final Message message) {
        try {
            HtmlPage page = getWebClient().getPage(FIRST_PAGE_URL);

            @SuppressWarnings("unchecked") final List<HtmlAnchor> detailLinks = (List<HtmlAnchor>) page.getByXPath(
                    "//table[@id='ctl00_ctl28_g_93cfe96e_ac88_4e1e_959a_6995c309bf5e_ctl00_grdListItems']/tbody/tr/td"
                            + "" + "/div/div/span/a");

            for (final HtmlAnchor detailLink : detailLinks) {
                createAndPublishMessage(DOMAIN_URL + detailLink.getHrefAttribute());
            }
        } catch (IOException e) {
            logger.error("Crawling failed with exception {}", e);
            throw new UnrecoverableException("Unable to crawl page", e);
        }
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
