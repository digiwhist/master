package eu.dl.utils;

import java.net.URLConnection;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Utils class that provides useful methods for work with files.
 *
 * @author Tomas Mrazek
 */
public final class FileUtils {

    private static final Logger logger = LoggerFactory.getLogger(FileUtils.class.getName());
    
    /**
     * Supress default constructor.
     */
    private FileUtils() {
    }

    /**
     * Attempts to determine file mimetype from the given filename. If the whole path is passed, the only filename is used for
     * determination.
     *
     * @see java.nio.file.Files#probeContentType(java.nio.file.Path)
     *
     * @param filename
     *      filename or path
     * @return mimetype or null
     */
    public static String getMimeType(final String filename) {
        try {
            return URLConnection.getFileNameMap().getContentTypeFor(filename);
        } catch (Exception ex) {
            logger.error("Unable to get mimetype for {}", filename, ex);
            return null;
        }
    }
}
