package eu.dl.worker.raw;

import com.gargoylesoftware.htmlunit.WebClient;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import eu.dl.core.UnrecoverableException;

import java.net.URL;
import java.time.LocalDate;

/**
 * Base class for incremental crawlers for paged HTTP sources.
 */
public abstract class BaseIncrementalPagedSourceHttpCrawler extends BaseIncrementalCrawler implements
        PagedSourceCrawler {
    private static final int HTTP_SUCCESS_CODE = 200;

    private final WebClient webClient;

    private int currentPageNumber = 1;
    private URL currentPageUrl;

    /**
     * Constructor. Web client is initialized and configured here.
     */
    protected BaseIncrementalPagedSourceHttpCrawler() {
        // configure web client
        webClient = new WebClient();
        webClient.getOptions().setUseInsecureSSL(true);
        webClient.getOptions().setThrowExceptionOnScriptError(false);
    }

    @Override
    protected final void initialSetup() {
        // no initial setup needed
        return;
    }

    @Override
    protected final void crawlSourceForDate(final LocalDate date) {
        setCurrentPageNumber(1);
        // get first page of list with results for given date
        HtmlPage actualPage = getSearchResultsStartPageForDate(date);

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
            logger.info("Processing page #{} with url {} and params {}", getCurrentPageNumber(), getCurrentPageUrl(),
                    actualPage.getWebResponse().getWebRequest().getRequestParameters());
            actualPage = extractDetailsFromPage(actualPage);

            // move to next page
            logger.debug("Data extracted, moving to next page.");
            actualPage = getNextPage(actualPage);

            // increase the counter
            setCurrentPageNumber(getCurrentPageNumber() + 1);
        }
    }

    @Override
    protected final void finalCleanup() {
        // no final cleanup needed
        return;
    }

    /**
     * Searches for increment records for given date and returns the first page of the results list to start crawling
     * from.
     *
     * @param incrementDate
     *         the date which the crawler should filter the search results for
     *
     * @return the start page for crawling the list of search results for given date (usually first page of the
     * search results list)
     */
    protected abstract HtmlPage getSearchResultsStartPageForDate(LocalDate incrementDate);

    /**
     * @return the currentPageNumber
     */
    protected final int getCurrentPageNumber() {
        return currentPageNumber;
    }

    /**
     * Returns the web client.
     *
     * @return web client
     */
    protected final WebClient getWebClient() {
        return webClient;
    }

    /**
     * @param currentPageNumber
     *         the currentPageNumber to set
     */
    protected final void setCurrentPageNumber(final int currentPageNumber) {
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
