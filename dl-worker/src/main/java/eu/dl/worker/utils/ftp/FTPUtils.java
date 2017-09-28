package eu.dl.worker.utils.ftp;

import java.time.DateTimeException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;

import org.apache.commons.net.ftp.FTPFile;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import eu.dl.core.UnrecoverableException;

/**
 * Created by tomasposepny.
 */
public final class FTPUtils {
    private static final Logger logger = LoggerFactory.getLogger(FTPUtils.class);

    /**
     * Suppress default constructor for noninstantiability.
     */
    private FTPUtils() {
        throw new AssertionError();
    }

    /**
     * Returns modification date of the given FTP file.
     *
     * @param file
     *         FTP file
     *
     * @return modification date
     */
    public static LocalDate getFtpFileModificationDate(final FTPFile file) {
        assert file != null;
        try {
            LocalDateTime dateTime = LocalDateTime.ofInstant(file.getTimestamp().toInstant(), ZoneId.systemDefault());
            return dateTime.toLocalDate();
        } catch (DateTimeException e) {
            logger.error("Unable to get file's modification date", e);
            throw new UnrecoverableException("Unable to get FTP file's modification date.");
        }
    }
}
