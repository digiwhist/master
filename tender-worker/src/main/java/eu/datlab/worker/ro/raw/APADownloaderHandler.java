package eu.datlab.worker.ro.raw;

import eu.dl.dataaccess.dto.raw.Raw;

import java.util.List;

/**
 * Downloader handler interface defines only one method 'handle'. This method returns list of raw records for the given url.
 */
public interface APADownloaderHandler {
    /**
     * @param url
     *      url to be downloaded
     * @return list of raw records
     */
    List<Raw> handle(String url);
}
