package eu.datlab.worker.fr.raw;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.datlab.worker.fr.BOAMPTenderUtils;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.RawDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.Message;
import eu.dl.worker.raw.downloader.BaseDownloader;
import eu.dl.worker.raw.utils.DownloaderUtils;
import eu.dl.worker.utils.textFactory.PlainTextService;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import static eu.datlab.worker.fr.BOAMPTenderUtils.ARCHIVE_URL_METADATA_KEY;
import static eu.datlab.worker.fr.BOAMPTenderUtils.FILE_PATH_METADATA_KEY;
import static eu.datlab.worker.fr.BOAMPTenderUtils.FILE_NAME_METADATA_KEY;
import static eu.datlab.worker.fr.BOAMPTenderUtils.HTML_SOURCE_DATA_METADATA_KEY;
import static eu.datlab.worker.fr.BOAMPTenderUtils.HTML_SOURCE_URL_METADATA_KEY;
import static org.apache.commons.codec.digest.DigestUtils.sha256Hex;

/**
 * Tender downloader for France, which saves XML file from FTP (is is saved on disc by crawler) and downloads HTML page
 * as metadata .
 *
 * @author Marek Mikes
 */
public class BOAMPTenderDownloader extends BaseDownloader<RawData> {
    private static final String VERSION = "1";

    @Override
    protected final boolean skipExisting(final Message message) {
        return false;
    }

    @Override
    public final RawDAO getRawDataDao() {
        return DAOFactory.getDAOFactory().getRawTenderDAO(getName(), getVersion());
    }

    @Override
    public final String getVersion() {
        return VERSION;
    }

    @Override
    protected final TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }

    @Override
    public final List<RawData> downloadAndPopulateRawData(final Message message) {
        // init raw data
        final RawData rawData = rawDao.getEmptyInstance();

        // get message parameters
        final String filePath = message.getValue(FILE_PATH_METADATA_KEY);
        final String fileName = message.getValue(FILE_NAME_METADATA_KEY);
        final String archiveUrl = message.getValue(ARCHIVE_URL_METADATA_KEY);

        // set source URL of archive on FTP where the XML file is
        try {
            rawData.setSourceUrl(new URL(archiveUrl));
        } catch (final MalformedURLException ex) {
            logger.error("Unable to download from malformed URL {}", archiveUrl);
            throw new UnrecoverableException("Unable to download data because of malformed url", ex);
        }
        // set source data of XML file
        try {
            rawData.setSourceData(PlainTextService.readFile(filePath, StandardCharsets.UTF_8));
        } catch (IOException e) {
            logger.error("File {} was not loaded", filePath);
            throw new UnrecoverableException("File was not loaded", e);
        }
        // set source file name
        rawData.setSourceFileName(fileName.substring(fileName.lastIndexOf('/') + 1));

        // set metadata (URL and content of HTML page)
        final String publicationSourceId = BOAMPTenderUtils.getPublicationSourceIdFrom(fileName);
        final String publicationWebUrl = String.format(BOAMPTenderUtils.PUBLICATION_PERMALINK_PATTERN,
                publicationSourceId);
        final HashMap<String, Object> metaData = new HashMap<>();
        metaData.put(HTML_SOURCE_URL_METADATA_KEY, publicationWebUrl);
        metaData.put(HTML_SOURCE_DATA_METADATA_KEY, DownloaderUtils.getResponseBody(publicationWebUrl));
        logger.info("Downloaded data from {}", publicationWebUrl);
        rawData.setMetaData(metaData);

        // set persistent id according to publication human readable URL, which is unique for each publication
        rawData.setPersistentId(getSourceId() + "_" + sha256Hex(publicationWebUrl));

        // delete successfully loaded file
        try {
            Files.delete(Paths.get(filePath));
        } catch (IOException e) {
            logger.error("File {} not deleted", filePath);
            throw new UnrecoverableException("File not deleted", e);
        }

        return Arrays.asList(rawData);
    }

    @Override
    protected final void postProcess(final RawData raw) {
    }
}
