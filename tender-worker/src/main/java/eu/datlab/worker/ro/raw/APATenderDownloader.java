package eu.datlab.worker.ro.raw;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.RawDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.raw.Raw;
import eu.dl.worker.Message;
import eu.dl.worker.raw.downloader.BaseDownloader;
import org.apache.commons.io.FilenameUtils;

import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLContext;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;
import java.io.File;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.List;

/**
 * Downloader for Romanian tenders.
 */
public class APATenderDownloader extends BaseDownloader {
    private static final String VERSION = "1.1";

    private static final String WORK_FOLDER = "RO_download";

    @Override
    protected final boolean skipExisting(final Message message) {
        return false;
    }

    @Override
    public final List downloadAndPopulateRawData(final Message message) {
        final List<Raw> rawData = new ArrayList<>();

        // Create a trust manager that does not validate certificate chains
        TrustManager[] trustAllCerts = new TrustManager[]{new X509TrustManager() {
            public X509Certificate[] getAcceptedIssuers() {
                return null;
            }

            public void checkClientTrusted(final X509Certificate[] certs, final String authType) {
            }

            public void checkServerTrusted(final X509Certificate[] certs, final String authType) {
            }
        }};

        // Install the all-trusting trust manager
        SSLContext sc = null;
        try {
            sc = SSLContext.getInstance("TLS");
            sc.init(null, trustAllCerts, new SecureRandom());
            HttpsURLConnection.setDefaultSSLSocketFactory(sc.getSocketFactory());
        } catch (NoSuchAlgorithmException | KeyManagementException e) {
            e.printStackTrace();
        }

        // get message parameters
        final String csvUrl = message.getValue("url");
        final String xlsxUrl = message.getValue("binaryDataUrl");

        // create work folder if necessary
        final File destinationFolder = new File(FilenameUtils.getName(WORK_FOLDER));
        destinationFolder.mkdir();

        APADownloaderHandler handler = APADownloaderHandlerFactory.getHandler(message);

        logger.info("File processing starts");

        if (handler != null) {
            List<Raw> result = handler.handle(handler instanceof CSVHandler ? csvUrl : xlsxUrl);
            rawData.addAll(result);
        } else {
            logger.error("Invalid message parameters combination");
            throw new UnrecoverableException("Unable to download file because of invalid message parameters combination");
        }

        logger.info("File processing finished");

        return rawData;
    }

    @Override
    public final String getVersion() {
        return VERSION;
    }

    @Override
    protected void postProcess(final Raw raw) {
    }

    @Override
    public final RawDAO getRawDataDao() {
        return DAOFactory.getDAOFactory().getRawTenderDAO(getName(), getVersion());
    }

    @Override
    protected final TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }
}