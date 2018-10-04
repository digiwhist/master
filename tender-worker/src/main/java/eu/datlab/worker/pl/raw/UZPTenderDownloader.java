package eu.datlab.worker.pl.raw;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.io.RandomAccessFile;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;

import eu.datlab.worker.downloader.BaseTenderFtpDownloader;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.Message;
import net.sf.sevenzipjbinding.IInArchive;
import net.sf.sevenzipjbinding.ISequentialOutStream;
import net.sf.sevenzipjbinding.SevenZip;
import net.sf.sevenzipjbinding.SevenZipException;
import net.sf.sevenzipjbinding.impl.RandomAccessFileInStream;
import net.sf.sevenzipjbinding.simple.ISimpleInArchive;
import net.sf.sevenzipjbinding.simple.ISimpleInArchiveItem;

/**
 * Downloads package from Poland FTP, unpack it, and finally extracts all files
 * in archive.
 *
 * @author Tomas Mrazek
 */
public final class UZPTenderDownloader extends BaseTenderFtpDownloader {
    private static final String VERSION = "2";
    private static final String WORK_FOLDER = "PL_workFolder";

    /**
     * Download and unpack package and extract all files in archive.
     *
     * @param message
     *         RabbitMQ message
     *
     * @return raw data
     */
    @Override
    protected List<RawData> downloadAndPopulateRawDataFromFtpServer(final Message message) {
        final List<RawData> rawData = new ArrayList<>();

        // get target from message
        final String packageUrl = message.getValue("url");
        String regex = null;
        boolean cache = false;
        if (message.getMetaData() != null) {
            regex = (String) message.getMetaData().get("regex");
            cache = (boolean) message.getMetaData().get("cache");
        }

        try {
            final URL url = new URL("ftp://ftp.uzp.gov.pl/" + packageUrl);

            // create work folder if necessary
            final File destinationFolder = new File(FilenameUtils.getName(WORK_FOLDER));
            destinationFolder.mkdir();

            // download file and store that with the same structure as in original
            final String archivePath = WORK_FOLDER + "/" + FilenameUtils.getName(packageUrl);
            final File tempArchiveFile = new File(archivePath);
            if (!tempArchiveFile.exists()) {
                logger.debug("Download data via FTP from url {}", url);
                final OutputStream osTempArchiveFile = new BufferedOutputStream(new FileOutputStream(tempArchiveFile));

                // download data
                getFtpClient().retrieveFile(packageUrl, osTempArchiveFile);
                osTempArchiveFile.close();
                logger.debug("Download data via FTP from url {} ... DONE", url);
            } else {
                logger.debug("Data for url {} loaded from cache");
            }

            rawData.addAll(unpack(archivePath, url, regex, cache));
        } catch (final Exception e) {
            logger.error("Unable to download and process data  {}", e);
            throw new UnrecoverableException("Unable to download and process data", e);
        }
        return rawData;
    }

    @Override
    public String getVersion() {
        return VERSION;
    }

    /**
     * Recursively unpack archive file with the given {@code path} and extarct all its files. In case that an extracted
     * file is EXE attempts also unpack this one instead of extracting. If unpacking of the nested EXE file fails only
     * error is logged.
     *
     * In some cases is necessary manually to set platform for SevenZip library. For this purpose exists
     * {@code sevenzip.platform} configuration property.
     * 
     * @param path
     *      archive file path
     * @param url
     *      url for published massages
     * @param regex
     *      regular expression for filtering of extracted files. Note it isn't used for nested EXE archives.
     * @param cache
     *      whether keep the archive file. Note it isn't used for nested EXE archives.
     * @return list of extracted raw data
     * @throws Exception in case that file downloading or extracting fails.
     */
    private List<RawData> unpack(final String path, final URL url, final String regex, final boolean cache)
        throws Exception {
        final List<RawData> extractedRawData = new ArrayList<>();
        // open downloaded file
        final RandomAccessFile archive = new RandomAccessFile(path, "r");

        // process files
        logger.debug("Unpack file {}", path);

        String platform = config.getParam("sevenzip.platform");
        if (platform != null && !platform.isEmpty()) {
            SevenZip.initSevenZipFromPlatformJAR(platform);
        }
        
        // open archive, autodetect archive type
        final IInArchive inArchive = SevenZip.openInArchive(null, new RandomAccessFileInStream(archive));

        // Getting simple interface of the archive inArchive
        final ISimpleInArchive simpleInArchive = inArchive.getSimpleInterface();

        // go through the archive file by file and store them in db
        for (final ISimpleInArchiveItem item : simpleInArchive.getArchiveItems()) {
            if (!item.isFolder()) {
                item.extractSlow(new ISequentialOutStream() {
                    @Override
                    public int write(final byte[] data) throws SevenZipException {
                        try {
                            logger.debug("Processing {} file {}", url, item.getPath());

                            if (regex != null && !item.getPath().matches(regex)) {
                                return data.length;
                            }
                            //nested exe archive
                            if (FilenameUtils.getExtension(item.getPath()).equals("exe")) {
                                final String pathExeArchive = WORK_FOLDER + "/" + item.getPath();
                                try {
                                    FileUtils.writeByteArrayToFile(new File(pathExeArchive), data);
                                    //append raw data from nested exe archive
                                    extractedRawData.addAll(unpack(pathExeArchive, url, null, false));
                                }  catch (Exception e) {
                                    logger.error("Unable to extract archive {} because of {}", pathExeArchive, e);
                                }

                                return data.length;
                            }

                            // convert data to string
                            final String stringData = new String(data, "UTF-8");

                            final RawData tender = new RawData();
                            tender.setSourceData(stringData);
                            tender.setSourceUrl(url);
                            tender.setSourceFileName(item.getPath());

                            extractedRawData.add(tender);

                            logger.info("New tender downloaded from url {}", url);
                        } catch (final UnsupportedEncodingException e) {
                            throw new UnrecoverableException("Unable to convert data to UTF-8", e);
                        }

                        return data.length;
                    }
                });
            }
        }

        archive.close();

        if (!cache) {
            // delete the file, its not necessary anymore
            final File deleteFile = new File(path);
            deleteFile.delete();
        }

        return extractedRawData;
    }
}
