package eu.digiwhist.worker.ge.raw;

import java.util.List;

import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlPage;

import eu.digiwhist.dataaccess.dao.DAOFactory;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.worker.Message;
import eu.dl.worker.MessageFactory;
import eu.dl.worker.raw.BasePagedSourceHttpCrawler;
import eu.dl.worker.raw.utils.CrawlerUtils;

/**
 * Asset declarations crawler for Georgia.
 *
 * @author Marek Mikes
 */
public final class GovGEAssetDeclarationCrawler extends BasePagedSourceHttpCrawler {
    private static final String VERSION = "1";
    private static final String START_PAGE_URL = "https://declaration.gov.ge/eng/searchDeclarations.php";
    private static final String NEXT_BUTTON_XPATH = "//div[@class='next']/div[@class='text']/a";

    /**
     * Default constructor.
     */
    public GovGEAssetDeclarationCrawler() {
        super();
        getWebClient().getOptions().setJavaScriptEnabled(false);
    }

    @Override
    public HtmlPage extractDetailsFromPage(final HtmlPage page) {
        final List<HtmlAnchor> declarationList = ((List<HtmlAnchor>) page.getByXPath(
                "//a[child::div[@class='info-content']]"));
        for (final HtmlAnchor declaration : declarationList) {
            String fileUrl = "https://declaration.gov.ge/eng/" + declaration.getHrefAttribute();

            final Message outgoingMessage = MessageFactory.getMessage();
            outgoingMessage.setValue("binaryDataUrl", fileUrl);
            publishMessage(outgoingMessage);
            logger.info("New message {} with url {} sent to be processed", outgoingMessage, fileUrl);
        }

        return page;
    }

    @Override
    protected HtmlPage getSearchResultsStartPage() {
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
