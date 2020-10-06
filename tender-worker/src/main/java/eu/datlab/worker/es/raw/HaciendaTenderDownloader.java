package eu.datlab.worker.es.raw;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.RawDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.Message;
import eu.dl.worker.raw.downloader.BaseDownloader;
import eu.dl.worker.utils.archive.ArchiveUtils;
import org.apache.commons.compress.archivers.ArchiveStreamFactory;


import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Downloads Spanish data from Hacienda source.
 */
public final class HaciendaTenderDownloader extends BaseDownloader<RawData> {

    private static final String VERSION = "1.0";

    @Override
    protected boolean skipExisting(final Message message) {
        return false;
    }

    @Override
    public List<RawData> downloadAndPopulateRawData(final Message message) {
        final List<RawData> result = new ArrayList<>();

        final String sourceDataUrl = message.getValue("url");
        HashMap<String, Object> metaData = message.getMetaData();

        if (sourceDataUrl == null || sourceDataUrl.isEmpty()) {
            logger.error("Invalid message - source url is empty");
            throw new UnrecoverableException("Unable to download file because of empty url");
        }

        InputStream inputStream;
        try {
            URL zipUrl = new URL(sourceDataUrl);
            inputStream = zipUrl.openStream();
        } catch (final MalformedURLException ex) {
            logger.error("Unable to download from malformed URL {}", sourceDataUrl);
            throw new UnrecoverableException("Unable to download data because of malformed url", ex);
        } catch (IOException ex) {
            logger.error("Unable to read file from {}", sourceDataUrl);
            throw new UnrecoverableException("Unable to read remote file");
        }

        // map of downloaded files from zip
        HashMap<String, String> extractedFiles =
                ArchiveUtils.extract(inputStream, sourceDataUrl, ArchiveStreamFactory.ZIP, null, StandardCharsets.UTF_8);

        if (extractedFiles.isEmpty()) {
            logger.warn("Archive seems to be empty: {}", sourceDataUrl);
            return result;
        }

        // save every file as separate rawData
        for (Map.Entry<String, String> atom : extractedFiles.entrySet()) {
            final RawData rawData = rawDao.getEmptyInstance();
            try {
                rawData.setSourceUrl(new URL(sourceDataUrl));
            } catch (MalformedURLException e) {
                e.printStackTrace();
            }
            // file's content is now in atom's value
            rawData.setSourceData(atom.getValue());
            if(metaData == null){
                metaData = new HashMap<>();
            }

            rawData.setMetaData(metaData);
            result.add(rawData);
        }

        return result;
    }

    @Override
    public RawDAO<RawData> getRawDataDao() {
        return DAOFactory.getDAOFactory().getRawTenderDAO(getName(), getVersion());
    }

    @Override
    public String getVersion() {
        return VERSION;
    }

    @Override
    protected TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }

    @Override
    protected void postProcess(final RawData raw) {

    }
}

