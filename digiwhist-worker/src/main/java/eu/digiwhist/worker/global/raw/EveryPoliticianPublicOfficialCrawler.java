package eu.digiwhist.worker.global.raw;

import java.io.IOException;
import java.util.List;

import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlElement;
import com.gargoylesoftware.htmlunit.html.HtmlPage;

import eu.digiwhist.dataaccess.dao.DAOFactory;
import eu.digiwhist.worker.global.PublicOfficialUtils.EuCountry;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.worker.Message;
import eu.dl.worker.raw.BaseHttpCrawler;

/**
 * Public officials crawler for Every Politician. Downloaded data are Json files.
 *
 * @author Michal Riha
 */
public final class EveryPoliticianPublicOfficialCrawler extends BaseHttpCrawler {
    private static final String VERSION = "1";
    private static final String FIRST_PAGE_URL = "http://everypolitician.org/countries.html";

    /**
     * Default constructor.
     */
    public EveryPoliticianPublicOfficialCrawler() {
        super();
        //javascript errors on page, not needed for crawling
        getWebClient().getOptions().setJavaScriptEnabled(false);
    }

    // First page has links to all countries, open links to countries we need, where is link to wanted Json file.
    @Override
    protected void doWork(final Message message) {
        try {
            final HtmlPage page = getWebClient().getPage(FIRST_PAGE_URL);

            // Get links to all countries on page.
            @SuppressWarnings("unchecked") final List<HtmlElement> linksToCountryDetail = (List<HtmlElement>) page
                    .getByXPath(
                    "//li/a/h3");

            for (HtmlElement linkToDetail : linksToCountryDetail) {
                // Filter to countries we need and open their detail page.
                if (EuCountry.contains(linkToDetail.getTextContent())) {
                    HtmlPage tempPage = linkToDetail.click();

                    // Acquire link to page(s) where we can download Json file.
                    @SuppressWarnings("unchecked") final List<HtmlAnchor> anchorsToDownloadPage = (List<HtmlAnchor>)
                            tempPage.getByXPath("//a[@class='button button--quarternary']");

                    for (HtmlAnchor anchorToDownloadPage : anchorsToDownloadPage) {
                        HtmlPage downloadPage = anchorToDownloadPage.click();

                        // Acquire link to Json file and send it to downloader.
                        HtmlAnchor anchorToDownload = downloadPage.getFirstByXPath(
                                "//div[@class='first-column']/p/a[@class='button button--download']");

                        createAndPublishMessage(anchorToDownload.getHrefAttribute());
                    }
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
