package eu.dl.worker.raw.downloader;

import com.gargoylesoftware.htmlunit.SgmlPage;
import com.gargoylesoftware.htmlunit.WebClient;
import com.gargoylesoftware.htmlunit.html.HtmlElement;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.raw.Raw;
import eu.dl.worker.Message;

import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.List;

/**
 * On Click HTTP downloader encapsulates functionality for downloading source
 * data that needs to be downloaded by clicking on a button (or link).
 * 
 * @param <T>
 *            item to be downloaded
 */
public abstract class BaseOnClickHttpDownloader<T extends Raw> extends BaseDownloader<T> {
    private static final int HTTP_SUCCESS_CODE = 200;

    private final WebClient webClient;

    /**
     * Public constructor initializes and configures web client.
     */
    public BaseOnClickHttpDownloader() {
        super();
        // configure web client
        webClient = new WebClient();
        webClient.getOptions().setUseInsecureSSL(true);
        webClient.getOptions().setThrowExceptionOnScriptError(false);
    }

    @Override
    public final List<T> downloadAndPopulateRawData(final Message message) {
        // init raw data
        final T rawData = rawDao.getEmptyInstance();

        // get message parameters
        final String sourceDataUrl = message.getValue("url");

        // download data and populate raw data object
        if (sourceDataUrl != null) {
            try {
                HtmlPage page = webClient.getPage(sourceDataUrl);

                // check, whether the page can be processed
                if (page.getWebResponse().getStatusCode() != HTTP_SUCCESS_CODE) {
                    logger.error("Received non-200 status code while trying to read from {}.", sourceDataUrl);
                    throw new UnrecoverableException("Dowloading failed. Non-200 status code received.");
                }

                final HtmlElement downloadButton = getDownloadButton(page);
                if (downloadButton != null) {
                    SgmlPage sourceData = getDownloadButton(page).click();
                    rawData.setSourceData(sourceData.getWebResponse().getContentAsString(StandardCharsets.UTF_8));
                    rawData.setSourceUrl(page.getUrl());
                } else {
                    logger.warn("Download button was not found on {}", sourceDataUrl);
                    throw new UnrecoverableException("Download button was not found.");
                }
            } catch (final Exception e) {
                logger.error("Downloading failed for page with url {} with exception {}", sourceDataUrl, e);
                throw new UnrecoverableException("Unable to download source data.", e);
            }
        } else {
            logger.error("No URL found in the message: {}", message);
            throw new UnrecoverableException("No URL provided.");
        }

        // return results
        return Arrays.asList(rawData);
    }

    /**
     * Getter for web client instance.
     *
     * @return web client instance
     */
    public final WebClient getWebClient() {
        return webClient;
    }

    /**
     * Returns HTML element (typically button) that should be clicked on to download source data.
     *
     * @param page
     *         source page
     *
     * @return clickable element for downloading source data
     */
    public abstract HtmlElement getDownloadButton(HtmlPage page);

    @Override
    protected final void postProcess(final T raw) {
    }
}
