package eu.datlab.worker.eu.raw;

import java.io.InputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.compress.archivers.ArchiveStreamFactory;
import org.apache.commons.compress.compressors.CompressorStreamFactory;
import org.apache.commons.io.FilenameUtils;

import eu.datlab.worker.downloader.BaseTenderFtpDownloader;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.Message;
import eu.dl.worker.utils.archive.ArchiveUtils;
import eu.dl.worker.utils.ftp.FTPFactory;

/**
 * Downloads daily package from TED ftp, unpack it, and finally extrats all
 * files in archive.
 *
 * @author Tomas Mrazek
 */
public final class TedTenderDownloader extends BaseTenderFtpDownloader {
    private static final String VERSION = "2";

    @Override
    protected boolean skipExisting(final Message message) {
        return false;
    }

    /**
     * Downloads and unpacks TED daily package and extract all files in archive.
     *
     * @param message
     *         RabbitMQ message
     *
     * @return raw data
     */
    @Override
    protected List<RawData> downloadAndPopulateRawDataFromFtpServer(final Message message) {
        final List<RawData> rawData = new ArrayList<>();
        final String fileUrl = message.getValue("url");
        URL url = null;

        try {
            if (FilenameUtils.getExtension(fileUrl.toLowerCase()).equals(CompressorStreamFactory.GZIP)) {
                url = new URL(FTPFactory.getFtpUrl(this.getName()) + "/" + fileUrl);

                logger.info("Unpacking daily package {}.", fileUrl);
                final InputStream dailyPackageStream = getFtpClient().retrieveFileStream(fileUrl);
                final HashMap<String, String> files = ArchiveUtils.extract(dailyPackageStream,
                        FilenameUtils.getName(fileUrl), ArchiveStreamFactory.TAR, CompressorStreamFactory.GZIP);

                for (final Map.Entry<String, String> file : files.entrySet()) {
                    logger.info("Extracting file {}.", file.getKey());

                    // init tender
                    final RawData tender = new RawData();
                    tender.setSourceData(file.getValue());
                    tender.setSourceUrl(url);
                    tender.setSourceFileName(file.getKey());

                    rawData.add(tender);

                    logger.info("New tender downloaded from url {}.", url);
                }
            }
        } catch (final Exception e) {
            logger.error("Downloading failed for daily package {}.", url, e);
            throw new UnrecoverableException("Daily package downloading failed.", e);
        }

        return rawData;
    }

    @Override
    public String getVersion() {
        return VERSION;
    }
}
