package eu.digiwhist.worker.be.raw;

import java.io.IOException;
import java.net.MalformedURLException;
import java.util.HashMap;
import java.util.List;

import com.gargoylesoftware.htmlunit.html.HtmlElement;
import com.gargoylesoftware.htmlunit.html.HtmlPage;

import eu.digiwhist.dataaccess.dao.DAOFactory;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.worker.Message;
import eu.dl.worker.raw.BaseHttpCrawler;

/**
 * Contracting authority crawler for Belgium.
 *
 * @author Michal Riha
 */
public class EnotContractingAuthorityCrawler extends BaseHttpCrawler {
    private static final String VERSION = "3";

    private static final String BASE_URL = "https://enot.publicprocurement.be/enot-war/";

    private static final String FIRST_PAGE = BASE_URL + "organisationTree" + ""
            + ".do?multiple=true2&holderId=undefined&simpleSearch=undefined";

    /**
     * Default constructor.
     */
    public EnotContractingAuthorityCrawler() {
        super();
        //there is broken JS dependency on the site, which was crashing crawler, fixed by lines under
        getWebClient().getOptions().setThrowExceptionOnFailingStatusCode(false);
        getWebClient().getOptions().setJavaScriptEnabled(false);
    }

    @Override
    protected final void doWork(final Message message) {
        try {
            HtmlPage page = getWebClient().getPage(FIRST_PAGE);
            sendAllSubfoldersToDownloader(page, null);
        } catch (IOException e) {
            logger.error("Crawling failed on page with exception {}", e);
            throw new UnrecoverableException("Unable to crawl page", e);
        }
    }

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    /**
     * Recursively looks for all closed folders on given page, send each folder as page with Url
     * to its parent to downloader.
     *
     * @param page
     *         page to look in
     * @param parentUrl
     *         if page is children of another folder, pass url
     *
     * @throws IOException
     *         when web client fails
     */
    private void sendAllSubfoldersToDownloader(final HtmlPage page, final String parentUrl) throws IOException {
        sendPageToDownloader(page, parentUrl);

        @SuppressWarnings("unchecked") List<HtmlElement> unexpandedDirectories = (List<HtmlElement>) page.getByXPath(
                "//li/ul[@class='ajax']/li");

        for (HtmlElement unexpandedDirectory : unexpandedDirectories) {
            sendAllSubfoldersToDownloader(page.getWebClient().getPage(BASE_URL + unexpandedDirectory.getTextContent()),
                    page.getUrl().toString());
        }
    }

    /**
     * Send given page with parent url metadata to downloader and write to log about it.
     *
     * @param page
     *         page to be saved
     * @param parentUrl
     *         url of pages parent
     *
     * @throws MalformedURLException
     *         incorrect url format
     */
    private void sendPageToDownloader(final HtmlPage page, final String parentUrl) throws MalformedURLException {
        if (parentUrl == null) {
            createAndPublishMessage(
                    page.getBaseURL().toString(),
                    page.getWebResponse().getContentAsString());
        } else {
            @SuppressWarnings("unchecked") final HashMap<String, Object> metadata = new HashMap();
            metadata.put("parentUrl", parentUrl);
            createAndPublishMessage(
                    page.getBaseURL().toString(),
                    page.getWebResponse().getContentAsString(),
                    metadata);
        }
        logger.info("Data from url published for downloader: {}", page.getBaseURL().toString());
    }

    @Override
    protected final TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }
}
