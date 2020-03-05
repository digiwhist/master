package eu.datlab.worker.ro.raw;

import eu.dl.worker.Message;

/**
 * APA downloader handler factory.
 */
public final class APADownloaderHandlerFactory {

    /**
     * Suppress default constructor.
     */
    private APADownloaderHandlerFactory() {
    }

    /**
     * @param message
     *      raw message
     * @return appropriate handler new instance
     */
    public static APADownloaderHandler getHandler(final Message message) {
        final String csvUrl = message.getValue("url");
        final String xlsxUrl = message.getValue("binaryDataUrl");

        if (csvUrl != null) {
            return new CSVHandler();
        } else if (xlsxUrl != null) {
            if (xlsxUrl.endsWith("xls")) {
                return new XLSHandler();
            } else {
                return new XLSXHandler();
            }
        }

        return null;
    }
}
