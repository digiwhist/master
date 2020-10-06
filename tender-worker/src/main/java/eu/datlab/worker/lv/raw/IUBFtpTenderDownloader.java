package eu.datlab.worker.lv.raw;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.compress.archivers.ArchiveStreamFactory;
import org.apache.commons.compress.compressors.CompressorStreamFactory;

import eu.datlab.worker.downloader.BaseTenderFtpDownloader;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.Message;
import eu.dl.worker.utils.archive.ArchiveUtils;

/**
 * Ftp tender crawler for IUB.
 *
 * @author Michal Riha
 */
public final class IUBFtpTenderDownloader extends BaseTenderFtpDownloader {
    private static final String VERSION = "2";

    @Override
    protected boolean skipExisting(final Message message) {
        return false;
    }

    @Override
    protected List<RawData> downloadAndPopulateRawDataFromFtpServer(final Message message) {
        final List<RawData> rawData = new ArrayList<>();

        logger.debug("Downloading source data from FTP server for message {}", message);

        final String url = message.getValue("url");

        try {
            final URI ftpUri = new URI(url);
            final InputStream inputStream = getFtpClient().retrieveFileStream(ftpUri.getPath());
            final HashMap<String, String> extractedFiles = ArchiveUtils.extract(inputStream, url,
                ArchiveStreamFactory.TAR, CompressorStreamFactory.GZIP);

            for (final Map.Entry<String, String> extractedFile : extractedFiles.entrySet()) {
                final RawData tender = new RawData();
                tender.setSourceData(extractedFile.getValue());
                tender.setSourceUrl(ftpUri.toURL());
                tender.setSourceFileName(extractedFile.getKey());

                rawData.add(tender);

                logger.info("Downloaded tender from url {}", extractedFile.getKey());
            }
        } catch (URISyntaxException | IOException e) {
            e.printStackTrace();
        }

        return rawData;
    }

    @Override
    public String getVersion() {
        return VERSION;
    }
}
