package eu.datlab.worker.global.raw;

import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.worker.Message;
import eu.dl.worker.raw.BaseHttpCrawler;

import java.io.IOException;
import java.util.List;

/**
 * Public officials crawler for Rulers.
 *
 * @author Michal Riha
 */
public final class RulersPublicOfficialCrawler extends BaseHttpCrawler {
    private static final String VERSION = "1.1";
    private static final String DOMAIN_URL = "http://rulers.org/";

    // On first page are links to rulers pages, on rulers pages are links to ministries and regions. We want all, so
    // we open rulers pages and find there links to the rest. Rulers sent to downloader as sourceData, rest only url.
    @Override
    protected void doWork(final Message message) {
        try {
            HtmlPage page = getWebClient().getPage(DOMAIN_URL);

            // find links to rulers pages.
            @SuppressWarnings("unchecked")
            final List<HtmlAnchor> linksToDetails = page.getByXPath("//a[starts-with(@href, 'rul')]");

            for (HtmlAnchor linkToDetails : linksToDetails) {
                // open each link to rulers page.
                page = linkToDetails.click();

                // publish link and sourceData to downloader
                createAndPublishMessage(
                        DOMAIN_URL + linkToDetails.getHrefAttribute(),
                        page.getWebResponse().getContentAsString());

                // find all links to ministries on current page and send them to downloader.
                @SuppressWarnings("unchecked")
                final List<HtmlAnchor> linksToMinistries = page.getByXPath("//a[contains(text(), 'Ministries, etc.')]");
                for (HtmlAnchor linkToMinistries : linksToMinistries) {
                    createAndPublishMessage(DOMAIN_URL + linkToMinistries.getHrefAttribute());
                }

                // find all links to regions on current page and send them to downloader.
                @SuppressWarnings("unchecked")
                final List<HtmlAnchor> linksToRegions = page.getByXPath("//a[contains(text(), 'Regions')]");
                for (HtmlAnchor linkToRegions : linksToRegions) {
                    createAndPublishMessage(DOMAIN_URL + linkToRegions.getHrefAttribute());
                }
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
