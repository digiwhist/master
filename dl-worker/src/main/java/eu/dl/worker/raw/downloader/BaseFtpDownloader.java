package eu.dl.worker.raw.downloader;

import java.io.IOException;
import java.net.SocketException;
import java.util.List;

import org.apache.commons.net.ftp.FTPClient;

import eu.dl.dataaccess.dto.raw.Raw;
import eu.dl.worker.Message;
import eu.dl.worker.utils.ftp.FTPFactory;

/**
 * Provides encapsulated functionality for all FTP workers. At first, it
 * connects to the FTP server, performs defined actions and finally disconnects
 * from the FTP server.
 * 
 * @param <T>
 *            item to be downloaded
 */
public abstract class BaseFtpDownloader<T extends Raw> extends BaseDownloader<T> {
    private FTPClient ftpClient;

    @Override
    public final List<T> downloadAndPopulateRawData(final Message message) {
        ftpClient = FTPFactory.getClientAndConnect(getName());
        logger.info("Connected to FTP server {}", ftpClient.getRemoteAddress());

        try {
            ftpClient.setSoTimeout(30000);
        } catch (SocketException e) {
            logger.error("Attempt to set timeout of a currently open connection failed.", e);
        }

        // do work
        List<T> rawData = downloadAndPopulateRawDataFromFtpServer(message);

        // logout and disconnect from FTP server
        try {
            if (ftpClient.isConnected()) {
                ftpClient.logout();
                ftpClient.disconnect();
            }
        } catch (IOException e) {
            logger.error("Disconnecting from FTP server failed.", e);
        }

        return rawData;
    }

    @Override
    protected final void postProcess(final T raw) {
    }

    /**
     * @return FTP client
     */
    protected final FTPClient getFtpClient() {
        return ftpClient;
    }

    /**
     * Processes message from crawler, downloads requested source data from FTP server and prepares raw data objects.
     *
     * @param message
     *         message from crawler containing parameters for downloading
     *
     * @return raw data objects populated with downloaded data
     */
    protected abstract List<T> downloadAndPopulateRawDataFromFtpServer(Message message);
}
