package eu.datlab.worker.ro.raw;

import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.DummyTransactionUtils;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.worker.Message;
import eu.dl.worker.MessageFactory;
import eu.dl.worker.raw.BaseHttpCrawler;

import java.io.IOException;
import java.util.List;

/**
 * Crawler for Romanian tenders.
 */
public class APATenderCrawler extends BaseHttpCrawler {
    private static final String VERSION = "1";
    private static final String[] PAGE_URLS = new String[]{
            "http://data.gov.ro/dataset/achizitii-publice-2007-2016-contracte6",
            "http://data.gov.ro/dataset/achizitii-publice-2010-2015-anunturi-de-participare",
            "http://data.gov.ro/dataset/achizitii-publice-2016",
            "http://data.gov.ro/dataset/achizitii-publice-2017",
            "http://data.gov.ro/dataset/achizitii-publice-2018",
            "http://data.gov.ro/dataset/achiziti-publice-2019"
    };

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final void doWork(final Message message) {

        for (String pageUrl : PAGE_URLS) {
            try {
                final HtmlPage resultPage = getWebClient().getPage(pageUrl);
                final List<HtmlAnchor> csvAndXlsLinks = resultPage.getByXPath("//a[@class='resource-url-analytics']");

                if (csvAndXlsLinks.isEmpty()) {
                    logger.error("Crawling failed, no files to crawl");
                    throw new UnrecoverableException("Crawling failed");
                }

                for (HtmlAnchor link : csvAndXlsLinks) {
                    if(link.getHrefAttribute().contains(".csv")){
                        createAndPublishMessage(link.getHrefAttribute());
                    } else {
                        final Message outgoingMessage = MessageFactory.getMessage();
                        outgoingMessage.setValue("binaryDataUrl", link.getHrefAttribute());
                        publishMessage(outgoingMessage);
                        logger.info("New message sent to be processed: {}", outgoingMessage);
                    }
                }


            } catch (IOException e) {
                logger.error("Crawling failed with exception {}", e);
                throw new UnrecoverableException("Crawling failed", e);
            }

        }
    }


    @Override
    protected final TransactionUtils getTransactionUtils() {
        return new DummyTransactionUtils();
    }
}
