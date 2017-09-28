package eu.dl.worker.raw;

import com.gargoylesoftware.htmlunit.WebClient;

/**
 * Base class for HTTP crawlers using HTMLUnit for crawling.
 */
public abstract class BaseHttpCrawler extends BaseCrawler {
    private final WebClient webClient;

    /**
     * Constructor. Web client is initialized and configured here.
     */
    protected BaseHttpCrawler() {
        super();
        // configure web client
        webClient = new WebClient();
        webClient.getOptions().setUseInsecureSSL(true);
        webClient.getOptions().setThrowExceptionOnScriptError(false);
    }

    /**
     * Returns the web client.
     *
     * @return web client
     */
    protected final WebClient getWebClient() {
        return webClient;
    }
}
