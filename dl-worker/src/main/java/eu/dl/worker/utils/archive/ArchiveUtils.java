package eu.dl.worker.utils.archive;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.RandomAccessFile;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import org.apache.commons.compress.archivers.ArchiveEntry;
import org.apache.commons.compress.archivers.ArchiveException;
import org.apache.commons.compress.archivers.ArchiveInputStream;
import org.apache.commons.compress.archivers.ArchiveStreamFactory;
import org.apache.commons.compress.compressors.CompressorException;
import org.apache.commons.compress.compressors.CompressorStreamFactory;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import eu.dl.core.UnrecoverableException;
import net.sf.sevenzipjbinding.ExtractOperationResult;
import net.sf.sevenzipjbinding.IInArchive;
import net.sf.sevenzipjbinding.SevenZip;
import net.sf.sevenzipjbinding.SevenZipException;
import net.sf.sevenzipjbinding.impl.RandomAccessFileInStream;
import net.sf.sevenzipjbinding.simple.ISimpleInArchive;
import net.sf.sevenzipjbinding.simple.ISimpleInArchiveItem;

/**
 * Provides utility functions for working with archives (eg. zip, tar, taz, ...).
 */
public final class ArchiveUtils {

    private static final Logger logger = LoggerFactory.getLogger(ArchiveUtils.class);

    private static final int BUFFER_SIZE = 4096;
    private static final String STREAM_ENCODING = "UTF-8";

    private static final String[] ARCHIVE_EXTENSIONS = new String[]{"zip", "tar", "taz", "tar.gz"};
    private static final Set<String> ARCHIVE_EXTENSIONS_SET = new HashSet<>(Arrays.asList(ARCHIVE_EXTENSIONS));

    private static final String WORK_FOLDER = "archiveUtils_workFolder";

    /**
     * Suppress default constructor for noninstantiability.
     */
    private ArchiveUtils() {
        throw new AssertionError();
    }

    /**
     * Extracts given archive and returns map with file names and content of all the included files. Archive format
     * and compression type will be auto-detected.
     *
     * @param inputStream
     *         archive input stream
     * @param fileName
     *         name of the archive file (including extension)
     * @return map of all the extracted file names with associated content
     */
    public static HashMap<String, String> extract(final InputStream inputStream, final String fileName) {
        return extract(inputStream, fileName, null, null);
    }

    /**
     * Extracts given archive and returns map with file names and content of all the included files. Archive format
     * and/or compression type can be provided, otherwise this method tries to auto-detect them.
     *
     * @param inputStream
     *         archive input stream
     * @param fileName
     *         name of the archive file (including extension)
     * @param archiveFormat
     *         archive type - one of the {@link ArchiveStreamFactory} constants or null for auto-detection
     * @param compressionType
     *         compression type - one of the {@link CompressorStreamFactory} constants or null for auto-detection
     * @return map of all the extracted file names with associated content
     * @throws ArchiveUnpackingException
     *         if the unpacking fails due to any reason
     */
    public static HashMap<String, String> extract(final InputStream inputStream, final String fileName,
                                                  final String archiveFormat, final String compressionType) {
        return extract(inputStream, fileName, archiveFormat, compressionType, StandardCharsets.UTF_8);
    }
    /**
     * Extracts given archive and returns map with file names and content of all the included files. Archive format
     * and/or compression type can be provided, otherwise this method tries to auto-detect them.
     *
     * @param inputStream
     *         archive input stream
     * @param fileName
     *         name of the archive file (including extension)
     * @param archiveFormat
     *         archive type - one of the {@link ArchiveStreamFactory} constants or null for auto-detection
     * @param compressionType
     *         compression type - one of the {@link CompressorStreamFactory} constants or null for auto-detection
     * @param charset
     *         file content encoding
     * @return map of all the extracted file names with associated content
     * @throws ArchiveUnpackingException
     *         if the unpacking fails due to any reason
     */
    public static HashMap<String, String> extract(final InputStream inputStream, final String fileName,
                                                  final String archiveFormat, final String compressionType,
                                                  final Charset charset) {
        assert inputStream != null;
        assert fileName != null;

        final HashMap<String, String> output = new HashMap<>();

        try (BufferedInputStream bufferedArchiveStream = new BufferedInputStream(inputStream);
             ArchiveInputStream archiveInputStream = getArchiveInputStream(bufferedArchiveStream, archiveFormat,
                     compressionType)) {
            ArchiveEntry entry;
            while ((entry = archiveInputStream.getNextEntry()) != null) {
                final String entryName = entry.getName();

                // check, whether the entry is again an archive and if so, extract recursively
                if (isKnownArchive(entryName)) {
                    output.putAll(extract(archiveInputStream, entryName));
                } else if (!entry.isDirectory()) {
                    // filters out directories, but files (even those inside directories) are processed
                    output.put(entryName, getEntryContent(archiveInputStream, charset));
                }
            }
            return output;
        } catch (IOException e) {
            logger.error("Error while reading extracted content from archive input stream.", e);
            throw new ArchiveUnpackingException("Reading extracted content failed.", e);
        } finally {
            try {
                inputStream.close();
            } catch (IOException e) {
                logger.error("Error while closing input stream.", e);
            }
        }
    }

    /**
     * Extracts archive (not recursively).
     *
     * @param inputStream
     *         archive input stream
     * @param fileName
     *         name of the archive file (including extension)
     * @return map of all the extracted file names with associated content
     */
    public static HashMap<String, String> extractUsing7zip(final InputStream inputStream, final String fileName) {

        final HashMap<String, String> extractedFiles = new HashMap<>();

        // create work folder if necessary
        final File destinationFolder = new File(FilenameUtils.getName(WORK_FOLDER));
        destinationFolder.mkdir();

        File tempArchiveFile = null;
        IInArchive inArchive = null;
        RandomAccessFile randomAccessFile = null;
        try {
            // create file on disk and convert input stream into the newly created file
            tempArchiveFile = new File(WORK_FOLDER, FilenameUtils.getName(fileName));
            Files.copy(inputStream, tempArchiveFile.toPath(), StandardCopyOption.REPLACE_EXISTING);

            // open created file
            randomAccessFile = new RandomAccessFile(tempArchiveFile, "r");

            // process files
            logger.debug("Unpack downloaded file {}", tempArchiveFile.getAbsolutePath());

            // open archive, autodetect archive type
            inArchive = SevenZip.openInArchive(null, new RandomAccessFileInStream(randomAccessFile));

            // Getting simple interface of the archive inArchive
            final ISimpleInArchive simpleInArchive = inArchive.getSimpleInterface();

            // go through the archive file by file
            for (final ISimpleInArchiveItem item : simpleInArchive.getArchiveItems()) {
                final StringBuilder stringDataBuilder = new StringBuilder();
                // filter out folders (but process files inside folders)
                logger.info(item.getPath());

                if (!item.isFolder()) {
                    ExtractOperationResult result = item.extractSlow(data -> {
                        try {
                            logger.debug("Processing {} file {}", fileName, item.getPath());

                            // convert data to string
                            stringDataBuilder.append(new String(data, "UTF-8"));
                        } catch (final UnsupportedEncodingException e) {
                            throw new UnrecoverableException("Unable to convert data to UTF-8 while extracting.", e);
                        }
                        return data.length;
                    });

                    if (result == ExtractOperationResult.OK) {
                        final String stringData = stringDataBuilder.toString();
                        //logger.info("file: {} \n data: {}", item.getPath(), stringData);

                        // add extracted file to output list
                        extractedFiles.put(item.getPath(), stringData);
                    } else {
                        logger.error("Error extracting archive: {}", result);
                        throw new UnrecoverableException("Unable to extractUsing7zip archive.");
                    }
                }
            }
            return extractedFiles;
        } catch (final Exception e) {
            logger.error("Unable to download and extractUsing7zip archive.", e);
            throw new UnrecoverableException("Unable to download and extractUsing7zip archive.", e);
        } finally {
            if (tempArchiveFile != null) {
                try {
                    // delete the file, its not necessary anymore
                    tempArchiveFile.delete();
                } catch (Exception e) {
                    logger.error("Error deleting temporary archive file.", e);
                }
            }
            if (inArchive != null) {
                try {
                    inArchive.close();
                } catch (SevenZipException e) {
                    logger.error("Error closing archive.", e);
                }
            }
            if (randomAccessFile != null) {
                try {
                    randomAccessFile.close();
                } catch (IOException e) {
                    logger.error("Error closing created random access file.", e);
                }
            }
        }
    }

    /**
     * Checks whether the file is known archive by its extension.
     *
     * @param fileName
     *         name of the archive file (including extension)
     * @return true if the provided file name has known archive extension
     */
    private static boolean isKnownArchive(final String fileName) {
        assert fileName != null;
        for (String extension : ARCHIVE_EXTENSIONS_SET) {
            if (fileName.endsWith(extension)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Creates an archive input stream from input stream. Archive type and compression type can be provided
     * explicitly, otherwise these are autodetected.
     *
     * @param inputStream
     *         input stream
     * @param archiveFormat
     *         archive type
     * @param compressionType
     *         compression type
     * @return archive input stream with archive content
     * @throws ArchiveUnpackingException
     *         if the archive or compressor input stream creation failed
     */
    private static ArchiveInputStream getArchiveInputStream(final InputStream inputStream, final String archiveFormat,
                                                            final String compressionType) {
        assert inputStream != null;

        try {
            InputStream compressorInputStream;

            if (compressionType != null) {
                // explicitly provided compression type
                compressorInputStream = new CompressorStreamFactory().createCompressorInputStream(compressionType,
                        inputStream);
            } else {
                try {
                    // auto-detecting the compressor type
                    compressorInputStream = new CompressorStreamFactory().createCompressorInputStream(inputStream);
                } catch (CompressorException e) {
                    logger.warn("Unable to autodetect compressor type, trying without compression.");
                    compressorInputStream = inputStream;
                }
            }

            if (archiveFormat != null) {
                // explicitly provided archive type
                return new ArchiveStreamFactory().createArchiveInputStream(archiveFormat, compressorInputStream);
            } else {
                // autodetecting the archive type
                return new ArchiveStreamFactory().createArchiveInputStream(
                        new BufferedInputStream(compressorInputStream));
            }
        } catch (CompressorException e) {
            logger.error("Compressor input stream creation failed because of invalid compressor name {}.",
                    compressionType, e);
            throw new ArchiveUnpackingException("Compressor input stream creation failed.", e);
        } catch (ArchiveException e) {
            logger.error("ArchiveUtils input stream creation failed because of invalid archive type {}.", archiveFormat,
                    e);
            throw new ArchiveUnpackingException("ArchiveUtils input stream creation failed,", e);
        }
    }

    /**
     * @param archiveInputStream
     *         input stream with archive content
     * @param charset
     *         archive content encoding
     * @return the content of given archive stream as text
     * @throws ArchiveUnpackingException
     *         if the stream cannot be read or some other I/O error occurs
     */
    private static String getEntryContent(final ArchiveInputStream archiveInputStream, final Charset charset) {
        try {
            final StringWriter writer = new StringWriter();
            IOUtils.copy(archiveInputStream, writer, charset);
            return writer.toString();
        } catch (IOException e) {
            logger.error("Error while reading content from archive input stream.", e);
            throw new ArchiveUnpackingException("Error while reading content from archive input stream", e);
        }
    }
}
