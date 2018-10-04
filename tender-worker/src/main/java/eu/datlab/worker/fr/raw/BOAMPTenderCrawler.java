package eu.datlab.worker.fr.raw;

import eu.datlab.worker.raw.BaseDatlabIncrementalFtpCrawler;
import eu.dl.core.RecoverableException;
import eu.dl.core.UnrecoverableException;
import eu.dl.worker.Message;
import eu.dl.worker.MessageFactory;
import eu.dl.worker.utils.archive.ArchiveUtils;
import eu.dl.worker.utils.ftp.FTPUtils;
import org.apache.commons.compress.archivers.ArchiveStreamFactory;
import org.apache.commons.net.ftp.FTPConnectionClosedException;
import org.apache.commons.net.ftp.FTPFile;

import static eu.datlab.worker.fr.BOAMPTenderUtils.ARCHIVE_URL_METADATA_KEY;
import static eu.datlab.worker.fr.BOAMPTenderUtils.FILE_PATH_METADATA_KEY;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.SocketException;
import java.net.SocketTimeoutException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.LocalDate;
import java.time.Month;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Tender crawler for France.
 *
 * @author Marek Mikes
 */
public final class BOAMPTenderCrawler extends BaseDatlabIncrementalFtpCrawler {
    private static final String VERSION = "1";

    private static final String CURRENT_YEARS_FTP_DIRECTORY_PATH = "/BOAMP/";

    // we already have old publications till 9.4.2017
    private static final LocalDate FIRST_DATE_AVAILABLE = LocalDate.of(2017, Month.APRIL, 9);

    private HashMap<LocalDate, List<String>> fileModificationDateUrlsMap;

    /**
     * Default constructor.
     */
    public BOAMPTenderCrawler() {
        super();
        fileModificationDateUrlsMap = new HashMap<>();
    }

    @Override
    protected void crawlSourceForDate(final LocalDate date) {
        logger.info("Crawling for date {}", date);
        List<String> urls = fileModificationDateUrlsMap.get(date);
        if (urls != null) {
            for (final String url : urls) {
                try {
                    final URI ftpUri = new URI(url);
                    final InputStream inputStream = getFtpClient().retrieveFileStream(ftpUri.getPath());

                    HashMap<String, String> extractedFiles;
                    assert date.isAfter(LocalDate.of(2015, Month.MARCH, 1));
                    // new .taz archives are zipped tars (.tar.zip)
                    extractedFiles = ArchiveUtils.extract(inputStream, url, ArchiveStreamFactory.ZIP, null,
                            StandardCharsets.UTF_8);

                    if (extractedFiles.isEmpty()) {
                        logger.warn("Archive seems empty: {}", url);
                    } else {
                        for (final Map.Entry<String, String> extractedFile : extractedFiles.entrySet()) {
                            // we want only XML files
                            if (!extractedFile.getKey().contains(".xml")) {
                                continue;
                            }

                            // save the XML file to temporary folder with name of archive (the name is in extracted
                            // file name)
                            final String filePath = "/tmp/fr_xml_files/" + extractedFile.getKey();
                            // save the file only when it does not exist
                            if (Files.notExists(Paths.get(filePath))) {
                                final String dirPath = filePath.substring(0, filePath.lastIndexOf('/'));
                                createDirIfNotExists(dirPath);
                                InputStream xmlFileStream = new ByteArrayInputStream(extractedFile.getValue().getBytes(
                                        "UTF-8"));
                                try {
                                    Files.copy(xmlFileStream, Paths.get(filePath));
                                } catch (IOException e) {
                                    logger.error("Unable to save file {}", e);
                                    throw new RecoverableException("Unable to save file", e);
                                }
                                logger.info("File content stored into \"{}\"", filePath);
                            }

                            final Message outgoingMessage = MessageFactory.getMessage();
                            outgoingMessage.setValue(FILE_PATH_METADATA_KEY, filePath);
                            outgoingMessage.setValue(ARCHIVE_URL_METADATA_KEY, ftpUri.toURL().toString());
                            publishMessage(outgoingMessage);
                            logger.info("New message sent to be processed: {}", outgoingMessage);
                        }
                    }

                    // finish the reading of archive, so we can start read the next one
                    inputStream.close();
                    if (!getFtpClient().completePendingCommand()) {
                        logger.error("We did not receive the completion reply from the server");
                        throw new UnrecoverableException("Unable to crawl the FTP server");
                    }
                } catch (SocketException e) {
                    logger.error("Downloading failed with SocketException: {}", e.getMessage());
                    throw new RecoverableException("Unable to download page because of SocketException", e);
                } catch (SocketTimeoutException e) {
                    logger.error("Downloading failed with SocketTimeoutException: {}", e.getMessage());
                    throw new RecoverableException("Unable to download page because of SocketTimeoutException", e);
                } catch (FTPConnectionClosedException e) {
                    logger.error("Downloading failed with FTPConnectionClosedException: {}", e.getMessage());
                    throw new RecoverableException("Unable to download page because of FTPConnectionClosedException",
                            e);
                } catch (IOException | URISyntaxException e) {
                    logger.error("Downloading failed with exception {}", e);
                    throw new UnrecoverableException("Unable to download page", e);
                }
            }
        }
    }

    @Override
    protected String getVersion() {
        return VERSION;
    }

    @Override
    protected LocalDate getDefaultStartDate() {
        return FIRST_DATE_AVAILABLE;
    }

    @Override
    protected ChronoUnit getIncrementUnit() {
        return ChronoUnit.DAYS;
    }

    @Override
    protected void sourceSpecificInitialFtpSetup() {
        assert fileModificationDateUrlsMap.isEmpty();
        try {
            getFtpClient().setSoTimeout(30000);

            scanAllFtpFilesLinks(CURRENT_YEARS_FTP_DIRECTORY_PATH + LocalDate.now().minusYears(1).getYear());
            scanAllFtpFilesLinks(CURRENT_YEARS_FTP_DIRECTORY_PATH + LocalDate.now().getYear());
        } catch (IOException e) {
            logger.error("Crawling (source specific initial ftp setup) failed with exception {}", e);
            throw new UnrecoverableException("Unable to crawl ftp source", e);
        }
    }

    @Override
    protected void sourceSpecificFinalFtpCleanup() {
        fileModificationDateUrlsMap.clear();
    }

    /**
     * Finds all files in folder/subfolders and set it to the crawler.
     *
     * @param path
     *         path to folder to search in
     * @throws IOException
     *         throws IOException when problem with ftp connection
     */
    private void scanAllFtpFilesLinks(final String path) throws IOException {
        logger.debug("Crawling folder: {}", path);
        final FTPFile[] files = getFtpClient().listFiles(path);
        for (final FTPFile file : files) {
            final String fileName = file.getName();

            // the file is really file or relative directories ("." and "..")
            assert file.isFile() || fileName.contentEquals(".") || fileName.contentEquals("..");

            if (file.isFile()) {
                final LocalDate modificationDate = FTPUtils.getFtpFileModificationDate(file);
                if (fileModificationDateUrlsMap.get(modificationDate) == null) {
                    fileModificationDateUrlsMap.put(modificationDate, new ArrayList<>(Arrays.asList(
                            String.format("ftp://%s%s/%s", getFtpClient().getRemoteAddress().getHostName(), path,
                                    file.getName()))));
                } else {
                    fileModificationDateUrlsMap.get(modificationDate).add(
                            String.format("ftp://%s%s/%s", getFtpClient().getRemoteAddress().getHostName(), path,
                                    file.getName()));
                }
            }
        }
    }

    /**
     * Creates directory at path if does not exist, including any necessary but nonexistent parent directories.
     *
     * @param path
     *            directory path
     *
     */
    private void createDirIfNotExists(final String path) {
        File directory = new File(String.valueOf(path));
        if (!directory.exists()) {
            directory.mkdirs();
        }
    }

}
