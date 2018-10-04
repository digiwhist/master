package eu.datlab.worker.global.raw;

import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import eu.datlab.dataaccess.dao.DAOFactory;
import eu.datlab.worker.global.PublicOfficialUtils.EuCountry;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.worker.Message;
import eu.dl.worker.MessageFactory;
import eu.dl.worker.raw.BaseHttpCrawler;

import java.io.IOException;
import java.util.List;

/**
 * Public officials crawler for CIA. Details are in PDF files, every country has several, divided by election period.
 *
 * @author Michal Riha
 */
public final class CIAPublicOfficialCrawler extends BaseHttpCrawler {
    private static final String VERSION = "1";
    private static final String DOMAIN_URL = "https://www.cia.gov";
    private static final String PUBLIC_OFFICIALS_URL = DOMAIN_URL + "/library/publications/world-leaders-1/";
    private static final String FIRST_PAGE_URL = PUBLIC_OFFICIALS_URL + "ID.html";

    // First page has links to all countries, open links to countries we need, where are link to wanted PDF files.
    @Override
    protected void doWork(final Message message) {
        try {
            HtmlPage page = getWebClient().getPage(FIRST_PAGE_URL);

            // Acquire links to countries detail page.
            final List<HtmlAnchor> linksToCountryDetail = page.getByXPath("//ul[@id='cosCountryList']/li/a");

            for (HtmlAnchor linkToDetail : linksToCountryDetail) {
                // Filter to countries we need.
                if (EuCountry.contains(linkToDetail.getTextContent())) {

                    // Open detail of country, where are links to needed PDF files.
                    page = linkToDetail.click();
                    final List<HtmlAnchor> linksToPdf = page.getByXPath("//div[@class='demo']/div/div/ul/li/a[starts-with(@href, 'pdfs')]");

                    // Send all links to pdf files on page to downloader.
                    for (HtmlAnchor linkToPdf : linksToPdf) {
                        final String pdfUrl = PUBLIC_OFFICIALS_URL + linkToPdf.getHrefAttribute();
                        final Message outgoingMessage = MessageFactory.getMessage();
                        outgoingMessage.setValue("binaryDataUrl", pdfUrl);
                        publishMessage(outgoingMessage);
                        logger.info("New message {} with url {} sent to be processed", outgoingMessage, pdfUrl);
                    }
                }
            }
        } catch (IOException e) {
            logger.error("Crawling failed for page url {} with exception {}", FIRST_PAGE_URL, e);
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
