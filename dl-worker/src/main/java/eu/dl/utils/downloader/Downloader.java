package eu.dl.utils.downloader;

import com.gargoylesoftware.htmlunit.Page;

import java.net.URL;
import java.util.function.Predicate;

/**
 * Provides download functionality for a special cases such a proxy, TOR etc.
 */
public interface Downloader {
    /**
     * Downloads page from url.
     *
     * @param url download target
     * @param isValid used to test if downloaded response is a valid one; repeats download if not
     *
     * @return downloaded page
     */
    Page getPage(URL url, Predicate<Page> isValid);
}
