package eu.dl.utils.downloader;

import com.gargoylesoftware.htmlunit.BrowserVersion;
import com.gargoylesoftware.htmlunit.Page;
import com.gargoylesoftware.htmlunit.ProxyConfig;
import com.gargoylesoftware.htmlunit.WebClient;
import eu.dl.core.config.Config;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.net.URL;
import java.util.function.Predicate;

/**
 * Provides downloader via Scrapoxy framework.
 * Handy for blocking sources, tries multiple times from different proxies before timeout.
 */
public final class ScrapoxyDownloader implements Downloader {

    /**
     * Main logger.
     */
    private Logger logger = LoggerFactory.getLogger(this.getClass().getName());

    /**
     * Application config instance.
     */
    private Config config = Config.getInstance();


    private WebClient webClient;

    /**
     * Default no of download tries.
     */
    public static final int DEFAULT_NO_OF_TRIES = 5;

    /**
     * Public constructor.
     */
    public ScrapoxyDownloader() {
        this.webClient = new WebClient(BrowserVersion.CHROME);
        webClient.getOptions().setUseInsecureSSL(true);
        webClient.getOptions().setRedirectEnabled(true);
        webClient.getOptions().setThrowExceptionOnFailingStatusCode(false);
        webClient.getOptions().setThrowExceptionOnScriptError(false);
        ProxyConfig proxyConfig = new ProxyConfig(
                config.getParam("scrapoxy.proxy_host"),
                config.getParamValueAs("scrapoxy.proxy_port", Integer::valueOf));
        webClient.getOptions().setProxyConfig(proxyConfig);
    }

    @Override
    public Page getPage(final URL url, final Predicate<Page> isValid) {
        for (int tryNo = 0; tryNo < DEFAULT_NO_OF_TRIES; tryNo++) {
            try {

                Page page = download(url);

                String proxyName = page.getWebResponse().getResponseHeaderValue("x-cache-proxyname");
                logger.debug("Downloaded via {}", proxyName);

                if(isValid.test(page)) {
                    return page;
                } else {
                    try {
                        Thread.sleep(1000 * (long) Math.pow(2, tryNo + 1));
                    } catch(InterruptedException e) {
                        e.printStackTrace();
                    }
                }

                logger.debug("Download {} failed, retry for {}n time", url, tryNo);

            } catch (IOException e) {
                logger.error("Unable to download {} with exception {}", url, e);
            }
        }

        return null;
    }

    /**
     * Tries to download the page.
     *
     * @param url url
     * @return downloaded page
     * @throws IOException in case of IO error
     */
    private Page download(final URL url) throws IOException {
        Page page = webClient.getPage(url);
        return page;
    }
}
