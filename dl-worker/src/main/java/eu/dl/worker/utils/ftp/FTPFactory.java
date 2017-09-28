package eu.dl.worker.utils.ftp;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

import org.apache.commons.net.ftp.FTP;
import org.apache.commons.net.ftp.FTPClient;
import org.apache.commons.net.ftp.FTPReply;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import eu.dl.core.RecoverableException;
import eu.dl.core.UnrecoverableException;
import eu.dl.core.config.Config;

/**
 * FTP factory which is responsible for creating and initializing FTP clients.
 */
public final class FTPFactory {

    private static final Logger logger = LoggerFactory.getLogger(FTPFactory.class);

    private static final String DEFAULT_USER = "anonymous";
    private static final String DEFAULT_PASSWORD = "";

    /**
     * Suppress default constructor for non-instantiability.
     */
    private FTPFactory() {
        throw new AssertionError();
    }

    /**
     * Provides new initialized FTP client connected to provided FTP server using anonymous (guest) mode.
     *
     * @param sourceClassName
     *         class name of the worker that needs to connect to FTP server
     *
     * @return initialized ftp client connected to the specified ftp server in anonymous (guest) mode
     */
    public static FTPClient getClientAndConnect(final String sourceClassName) {
        Config config = Config.getInstance();
        String ftpUrl = config.getParam(sourceClassName + ".ftpUrl");
        String ftpUser = config.getParam(sourceClassName + ".ftpUser");
        String ftpPassword = config.getParam(sourceClassName + ".ftpPassword");

        // if username is not provided, use anonymous login
        if (ftpUser == null) {
            ftpUser = DEFAULT_USER;
            ftpPassword = DEFAULT_PASSWORD;
        }

        return getClientAndConnect(ftpUrl, ftpUser, ftpPassword);
    }

    /**
     * @param sourceClassName
     *         class name of the worker
     * @return ftp url
     */
    public static String getFtpUrl(final String sourceClassName) {
        return Config.getInstance().getParam(sourceClassName + ".ftpUrl");
    }

    /**
     * Provides new initialized FTP client connected and logged in to specified FTP server using provided URI and
     * credentials.
     *
     * @param ftpUrl
     *         url of the ftp server
     * @param userName
     *         login user name
     * @param password
     *         login password
     *
     * @return initialized ftp client connected a logged to specified ftp server
     * @throws UnrecoverableException
     *         if the given url string violates RFC 2396
     * @throws RecoverableException
     *         if unable to establish connection to provided ftp server or unable to use anonymous mode
     */
    private static FTPClient getClientAndConnect(final String ftpUrl, final String userName, final String password) {
        final FTPClient ftpClient = new FTPClient();

        try {
            final URI ftpUri = new URI(ftpUrl);
            ftpClient.connect(ftpUri.getHost());
            ftpClient.login(userName, password);
        } catch (URISyntaxException e) {
            logger.error("Provided URL string {} could not be parsed as URI.", ftpUrl);
            throw new UnrecoverableException("Provided URL string could not be parsed as URI.", e);
        } catch (IOException e) {
            logger.error("Error while trying to connect to ftp url {}", ftpUrl, e);
            throw new RecoverableException("Unable to connect to ftp server", e);
        }

        if (!FTPReply.isPositiveCompletion(ftpClient.getReplyCode())) {
            logger.error("Connecting to ftp server was unsuccessful with reply code {}", ftpClient.getReplyCode());
            throw new RecoverableException("Connecting to ftp server was unsuccessful.");
        }

        // passive mode and file type, proper way for data connection:
        // http://www.codejava.net/java-se/networking/ftp/java-ftp-file-download-tutorial-and-example
        ftpClient.enterLocalPassiveMode();
        try {
            ftpClient.setFileType(FTP.BINARY_FILE_TYPE);
        } catch (IOException e) {
            logger.warn("Unable to set the file type for FTP client.");
        }
        return ftpClient;
    }
}
