package eu.dl.worker.raw;

import java.io.IOException;

import org.apache.commons.net.ftp.FTPClient;
import org.apache.commons.net.ftp.FTPFile;

import eu.dl.core.UnrecoverableException;
import eu.dl.worker.utils.ftp.FTPFactory;

/**
 * Base class for incremental FTP crawlers. Takes care for connecting to and disconnecting from FTP server.
 */
public abstract class BaseIncrementalFtpCrawler extends BaseIncrementalCrawler {
    private FTPClient ftpClient;

    @Override
    protected final void initialSetup() {
        // setup FTP client and connect to FTP server
        ftpClient = FTPFactory.getClientAndConnect(getName());

        sourceSpecificInitialFtpSetup();

        logger.info("Connected to FTP server {}", ftpClient.getRemoteAddress());
    }

    @Override
    protected final void finalCleanup() {
        // logout and disconnect from FTP server
        try {
            if (ftpClient.isConnected()) {
                ftpClient.logout();
                ftpClient.disconnect();
            }
        } catch (IOException e) {
            logger.error("Disconnecting from FTP server failed.", e);
        }

        sourceSpecificFinalFtpCleanup();
    }

    /**
     * @return FTP client
     */
    protected final FTPClient getFtpClient() {
        return ftpClient;
    }

    /**
     * Finds all files recursively in given folder. Optionally accepts only files that match the regular expression.
     *
     * @param dir
     *         explored folder
     * @param regex
     *         regular expression for file filtering by name. If is null all files are accepted
     */
    protected final void searchFiles(final String dir, final String regex) {
        try {
            for (final FTPFile file : getFtpClient().listFiles(dir)) {
                final String path = dir + file.getName();

                if (file.isDirectory()) {
                    this.searchFiles(path + "/", regex);
                } else if (file.isFile() && (regex == null || file.getName().matches(regex))) {
                    createAndPublishMessage(path);
                }
            }
        } catch (final IOException e) {
            logger.error("Extracting daily packages for {} failed with exception {}", dir, e);
            throw new UnrecoverableException("Extracting daily packages fails.", e);
        }
    }

    /**
     * Finds all packages recursively in given folder.
     *
     * @param dir
     *         explored folder
     */
    protected final void searchFiles(final String dir) {
        searchFiles(dir, null);
    }

    /**
     * Initial source specific FTP setup if needed before the actual work is done.
     */
    protected abstract void sourceSpecificInitialFtpSetup();

    /**
     * Cleaning after the source specific FTP work has been done. Might or might not be needed.
     */
    protected abstract void sourceSpecificFinalFtpCleanup();
}
