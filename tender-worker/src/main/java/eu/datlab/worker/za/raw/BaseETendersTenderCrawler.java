package eu.datlab.worker.za.raw;

import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlElement;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import eu.datlab.dataaccess.dao.DAOFactory;
import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.worker.Message;
import eu.dl.worker.raw.BasePagedSourceHttpCrawler;

import java.io.IOException;
import java.util.List;

/**
 * Base class for ZA crawlers.
 */
abstract class BaseETendersTenderCrawler extends BasePagedSourceHttpCrawler {
    private static final String VERSION = "1.0";

    /**
     * A.
     */
    BaseETendersTenderCrawler() {
        super();
        getWebClient().getOptions().setTimeout(9000000);
    }

    @Override
    protected HtmlPage getSearchResultsStartPage(final Message message) {
        try {
            return getWebClient().getPage(getInitialWebPage());
        } catch (IOException e) {
            logger.error("Unable to open result page with error: {}", e);
            throw new UnrecoverableException("Unable to open result page with error: {}", e);
        }
    }

    /**
     * Should return initial page url.
     * @return String
     */
    protected abstract String getInitialWebPage();

    @Override
    protected String getVersion() {
        return VERSION;
    }

    @Override
    protected TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }

    @Override
    public HtmlPage getNextPage(final HtmlPage actualPage) {
        final HtmlAnchor nextPageButton = actualPage.getFirstByXPath("//a[@title='Go to next page']");

        try {
            return nextPageButton == null ? null : getWebClient()
                    .getPage(PublicationSources.ZA_ETENDERS + nextPageButton.getHrefAttribute());
        } catch (IOException e) {
            logger.error("Unable to open next page with error: {}", e);
            throw new UnrecoverableException("Unable to open next page with error: {}", e);
        }
    }

    @Override
    public boolean isPageValid(final HtmlPage page) {
        return true;
    }

    /**
     * Get text of td.
     *
     * @param tdNumber td number
     * @param element element
     * @return String
     */
    String getTextOf(final int tdNumber, final HtmlElement element) {
        final List<HtmlElement> resultElements = element.getByXPath("td");
        return resultElements.size() <= tdNumber ? null : resultElements.get(tdNumber).getTextContent();
    }


}
