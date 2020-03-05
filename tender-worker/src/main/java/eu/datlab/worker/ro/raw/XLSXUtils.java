package eu.datlab.worker.ro.raw;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.raw.Raw;
import eu.dl.dataaccess.dto.raw.RawData;
import org.apache.commons.io.IOUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.channels.Channels;
import java.nio.channels.FileChannel;
import java.nio.channels.ReadableByteChannel;
import java.util.List;

/**
 * Utils class with methods useful for xls(x) processing.
 */
public final class XLSXUtils {
    private static final Logger logger = LoggerFactory.getLogger(XLSXUtils.class.getName());

    /**
     * Folder for saving the data.
     */
    public static final String WORK_FOLDER = "RO_download";

    /**
     * Suppress default constructor.
     */
    private XLSXUtils() {
    }

    /**
     * Inserts row to the given workbook.
     *
     * @param wb
     *      workbook
     * @param data
     *      row to be added
     * @param index
     *      row index
     */
    public static void insertRow(final Workbook wb, final List<String> data, final Integer index) {
        Sheet sheet = wb.getSheetAt(0);
        Row row = sheet.createRow(index != null ? index : (sheet.getLastRowNum() + 1));

        for (int i = 0; i < data.size(); i++) {
            Cell cell = row.createCell(i, CellType.STRING);
            cell.setCellValue(data.get(i));
        }
    }

    /**
     * @param header
     *      header row
     * @param clazz
     *      required class of workbook
     * @return workbook with sheet and header row
     */
    public static Workbook initWorkbook(final List<String> header, final Class<? extends Workbook> clazz) {
        try {
            Workbook workbook = clazz.getDeclaredConstructor().newInstance();

            Sheet sheet = workbook.createSheet();
            // header row
            insertRow(workbook, header, 0);

            return workbook;
        } catch (Exception e) {
            logger.error("Unable to get instance of workbook class {}", clazz.getName(), e);
            throw new UnrecoverableException("Unable to get instance of workbook class", e);
        }
    }

    /**
     * Appends row to the given workbook.
     *
     * @param wb
     *      workbook
     * @param data
     *      row to be added
     */
    public static void appendRow(final Workbook wb, final List<String> data) {
        insertRow(wb, data, null);
    }

    /**
     * For the given header row and data rows creates XLSX document and returns it as raw record.
     *
     * @param header
     *      header row
     * @param rows
     *      data rows
     * @param clazz
     *      required class of workbook
     * @return raw record
     */
    public static Raw getRawRecord(final List<String> header, final List<List<String>> rows, final Class<? extends Workbook> clazz) {
        Workbook workbook = XLSXUtils.initWorkbook(header, clazz);
        rows.forEach(r -> XLSXUtils.appendRow(workbook, r));

        File tmp = new File(WORK_FOLDER + "/fragment");
        Raw raw;
        try {
            FileOutputStream fileOut = new FileOutputStream(tmp);
            workbook.write(fileOut);
            fileOut.close();
            workbook.close();

            FileInputStream is = new FileInputStream(tmp);
            raw = getEmptyRaw();
            raw.setSourceBinaryData(IOUtils.toByteArray(is));

            is.close();
        } catch (Exception e) {
            logger.error("Unable to save xlsx fragment", e);
            throw new UnrecoverableException("Unable to save xlsx fragment");
        } finally {
            if (tmp.isFile()) {
                tmp.delete();
            }
        }

        return raw;
    }

    /**
     * Downloads file from given url and save it as 'workbook' in {@value WORK_FOLDER}.
     *
     * @param url
     *      url to be downloaded
     * @return path of saved file
     */
    public static String downloadWorkbook(final String url) {
        String wbPath = WORK_FOLDER + "/workbook";
        InputStream inputStream = null;

        try {
            final URL binaryDataUrl = new URL(url);
            inputStream = binaryDataUrl.openStream();

            ReadableByteChannel readableByteChannel = Channels.newChannel(inputStream);
            FileOutputStream fileOutputStream = new FileOutputStream(wbPath);
            FileChannel fileChannel = fileOutputStream.getChannel();
            fileChannel.transferFrom(readableByteChannel, 0, Long.MAX_VALUE);
        } catch (final MalformedURLException e) {
            logger.error("Unable to download from malformed URL {}", url);
            throw new UnrecoverableException("Unable to download file because of malformed url", e);
        } catch (IOException e) {
            logger.error("Unable to save file from {}", url, e);
            throw new UnrecoverableException("Unable to save file", e);
        } finally {
            if (inputStream != null) {
                try {
                    inputStream.close();
                } catch (IOException e) {
                    logger.error("Failed to close input stream.");
                }
            }
        }

        return wbPath;
    }

    /**
     * @param header
     *      header row
     * @param rows
     *      data rows
     * @param url
     *      url of the document
     * @param contentType
     *      content type of the document
     * @param index
     *      fragment's index
     * @param clazz
     *      workbook class
     * @return fragment raw record
     */
    public static Raw getFragmentRawRecord(final List<String> header, final List<List<String>> rows, final URL url,
                                    final String contentType, final int index, final Class<? extends Workbook> clazz) {
        Raw raw = getRawRecord(header, rows, clazz);

        try {
            raw.setSourceUrl(new URL(url, "?fragment=" + index));
            raw.setSourceDataMimeType(contentType);
        } catch (MalformedURLException e) {
            logger.error("Fragment URL is malformed", e);
            throw new UnrecoverableException("Fragment URL is malformed");
        }

        return raw;
    }

    /**
     * @return new empty istance of raw data.
     */
    public static RawData getEmptyRaw() {
        return new RawData();
    }
}
