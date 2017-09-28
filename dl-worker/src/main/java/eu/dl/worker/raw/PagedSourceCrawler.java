package eu.dl.worker.raw;

import com.gargoylesoftware.htmlunit.html.HtmlPage;

/**
 * Paged source crawlers should implement this interface.
 */
public interface PagedSourceCrawler {
    /**
     * Get next results page.
     *
     * @param actualPage
     *         actual page
     *
     * @return HtmlPage next page with search results
     */
    HtmlPage getNextPage(HtmlPage actualPage);

    /**
     * Extracts details from given list page and prepares RabbitMQ messages for downloaders or parsers.
     * If the page contains links to details, these links are extracted and sent to the appropriate downloader.
     * In other cases the method might prepare messages designated directly for the parsed.
     *
     * @param page
     *         current list page
     *
     * @return The same list page that has been sent to this method as {@code page} parameter. This is necessary for
     * cases when the crawler cannot keep the list page in a variable, because it gets invalidated during the details
     * extraction process (eg. Spanish source).
     */
    HtmlPage extractDetailsFromPage(HtmlPage page);

    /**
     * Returns true if the page is a valid search results page (list page). This method is useful for sources, where
     * the next button is active even on the last page and therefore it's necessary to check whether it returns valid
     * page or not.
     *
     * @param page
     *         current list page
     *
     * @return true if the page contains valid search results, false otherwise
     */
    boolean isPageValid(HtmlPage page);
}
