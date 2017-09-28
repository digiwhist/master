package eu.digiwhist.worker.ro.raw;

import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.DummyTransactionUtils;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.worker.Message;
import eu.dl.worker.raw.BaseHttpCrawler;

import java.io.IOException;
import java.util.List;

/**
 * Crawler for Romanian tenders.
 */
public class APATenderCrawler extends BaseHttpCrawler {
    private static final String VERSION = "1";
    private static final String PAGE_URL = "http://data.gov.ro/dataset/achizitii-publice-2007-2016-contracte6";

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final void doWork(final Message message) {
        try {
            final HtmlPage resultPage = getWebClient().getPage(PAGE_URL);
            @SuppressWarnings("unchecked") final List<HtmlAnchor> csvLinks = (List<HtmlAnchor>) resultPage
                    .getByXPath("//a[@class='resource-url-analytics']");

            if (csvLinks.isEmpty()) {
                logger.error("Crawling failed, no files to crawl");
                throw new UnrecoverableException("Crawling failed");
            }

            for (HtmlAnchor csvLink : csvLinks) {
                createAndPublishMessage(csvLink.getHrefAttribute());
            }
        } catch (IOException e) {
            logger.error("Crawling failed with exception {}", e);
            throw new UnrecoverableException("Crawling failed", e);
        }
    }

    @Override
    protected final TransactionUtils getTransactionUtils() {
        return new DummyTransactionUtils();
    }
}
