package eu.dl.worker.raw;

import java.net.URL;

import com.gargoylesoftware.htmlunit.html.HtmlPage;

import eu.dl.core.UnrecoverableException;
import eu.dl.worker.Message;

/**
 * Generic crawler for paged HTTP sources where the results are in form of paged list.
 */
public abstract class BasePagedSourceHttpCrawler extends BaseHttpCrawler implements PagedSourceCrawler {
    private static final int HTTP_SUCCESS_CODE = 200;

    private int currentPageNumber = 1;
    private URL currentPageUrl;

    @Override
    public final void doWork(final Message message) {
        setCurrentPageNumber(1);
        // get first page of list with results for given date
        HtmlPage actualPage = getSearchResultsStartPage(message);

        // go through all the list pages and extract details from each
        while (actualPage != null && isPageValid(actualPage)) {
            setCurrentPageUrl(actualPage.getUrl());

            // check for valid response
            if (actualPage.getWebResponse().getStatusCode() != HTTP_SUCCESS_CODE) {
                logger.error("Received non-200 status code while trying to read from {}.", getCurrentPageUrl());
                throw new UnrecoverableException("Crawling failed due to non-200 HTTP response.");
            }

            // process detail pages - download or send to downloader via message queue
            // the same page is returned from extracting details - this is necessary for cases when the crawler
            // cannot keep the list page in a variable, because it gets invalidated during the details extraction
            // process (eg. Spanish source).
            logger.info("Processing page #{} with url {}", getCurrentPageNumber(), getCurrentPageUrl());
            actualPage = extractDetailsFromPage(actualPage);

            // move to next page
            logger.debug("Data extracted, moving to next page.");
            actualPage = getNextPage(actualPage);

            // increase the counter
            setCurrentPageNumber(getCurrentPageNumber() + 1);
        }
    }

    /**
     * Returns the first page of the list to start crawling from.
     *
     * @param message
     *      incoming message
     * @return the start page for crawling the list - usually first page of the results list
     */
    protected abstract HtmlPage getSearchResultsStartPage(final Message message);

    /**
     * @return the currentPageNumber
     */
    protected final int getCurrentPageNumber() {
        return currentPageNumber;
    }

    /**
     * @param currentPageNumber
     *         the currentPageNumber to set
     */
    private void setCurrentPageNumber(final int currentPageNumber) {
        this.currentPageNumber = currentPageNumber;
    }

    /**
     * @return the URL of current page
     */
    protected final URL getCurrentPageUrl() {
        return currentPageUrl;
    }

    /**
     * @param currentPageUrl
     *         URL of current page to set
     */
    private void setCurrentPageUrl(final URL currentPageUrl) {
        this.currentPageUrl = currentPageUrl;
    }
}
