package eu.datlab.worker.ro.raw;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.raw.Raw;
import eu.dl.worker.raw.utils.DownloaderUtils;
import eu.dl.worker.utils.StringUtils;
import org.jsoup.Connection;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

/**
 * Downloads CSV file.
 */
public final class CSVHandler implements APADownloaderHandler {
    private final Logger logger = LoggerFactory.getLogger(this.getClass().getName());

    private static final int ROWS_COUNT_LIMIT = 10000;

    /**
     * @param url
     *      url to be downloaded
     * @return list of raw records
     */
    @Override
    public List<Raw> handle(final String url) {
        final Connection.Response response = DownloaderUtils.getUrlResponse(url);

        final String rawCSV = DownloaderUtils.getResponseBody(response);

        final String head = StringUtils.head(rawCSV, 1);

        List<Raw> result = new ArrayList<>();
        try {
            StringUtils.chunkStringByLines(rawCSV, ROWS_COUNT_LIMIT, 1)
                .forEach(n -> {
                    URL sourceUrl;
                    try {
                        sourceUrl = new URL(url + "?fragment=" + result.size() + 1);
                    } catch (final MalformedURLException ex) {
                        logger.error("Unable to download from malformed URL {}", url);
                        throw new UnrecoverableException("Unable to download data because of malformed url", ex);
                    }

                    Raw raw = XLSXUtils.getEmptyRaw();
                    raw.setSourceUrl(sourceUrl);
                    raw.setSourceData(head + "\n" + n);
                    raw.setSourceDataMimeType(response.contentType());

                    result.add(raw);
                });
        } catch (IOException e) {
            logger.error("Unable to split csv file from {}", url);
            throw new UnrecoverableException("Unable to split csv file");
        }

        logger.info("CSV file from {} downloaded", url);
        return result;
    }
}
