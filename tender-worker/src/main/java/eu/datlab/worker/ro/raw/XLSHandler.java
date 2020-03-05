package eu.datlab.worker.ro.raw;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.raw.Raw;
import org.apache.poi.hssf.eventusermodel.HSSFEventFactory;
import org.apache.poi.hssf.eventusermodel.HSSFRequest;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.poifs.filesystem.POIFSFileSystem;
import org.apache.poi.ss.usermodel.DateUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * Downloads XLS file.
 */
public final class XLSHandler implements APADownloaderHandler {
    private static final Logger logger = LoggerFactory.getLogger(XLSHandler.class.getName());

    private URL binaryDataUrl;
    private static final String CONTENT_TYPE = "application/vnd.ms-excel";

    private static final int ROWS_COUNT_LIMIT = 1000;

    private static final SimpleDateFormat DATETIME_FORMAT = new SimpleDateFormat("uuuu-MM-dd HH:mm:ss");

    /**
     * @param url
     *      url to be downloaded
     * @return list of raw records
     */
    @Override
    public List<Raw> handle(final String url) {
        List<Raw> rawData = new ArrayList<>();
        InputStream inputStream = null;

        String wbPath = XLSXUtils.downloadWorkbook(url);

        try {
            binaryDataUrl = new URL(url);

            List<String> header = new ArrayList<>();
            List<List<String>> rows = new ArrayList<>();

            // create a new file input stream with the input file
            FileInputStream fin = new FileInputStream(wbPath);
            // create a new org.apache.poi.poifs.filesystem.Filesystem
            POIFSFileSystem poifs = new POIFSFileSystem(fin);
            // get the Workbook (excel part) stream in a InputStream
            InputStream din = poifs.createDocumentInputStream("Workbook");
            // construct out HSSFRequest object
            HSSFRequest req = new HSSFRequest();
            // lazy listen for ALL records with the listener shown above
            req.addListenerForAllRecords(new HSSFParser((num, data) -> {
                if (num == 1) {
                    header.addAll(data);
                } else {
                    if (rows.size() == ROWS_COUNT_LIMIT) {
                        rawData.add(getRawRecord(header, rows, rawData.size() + 1));
                        rows.clear();
                    } else {
                        // third column is datetime, but the value is parsed as number, convert it
                        data.set(2, excelNumberToDateTime(data.get(2)));
                        rows.add(data);
                    }
                }
            }));
            // create our event factory
            HSSFEventFactory factory = new HSSFEventFactory();
            // process our events based on the document input stream
            factory.processEvents(req, din);
            // once all the events are processed close our file input stream
            fin.close();
            // and our document input stream (don't want to leak these!)
            din.close();

            poifs.close();

            // save rest of rows
            if (!rows.isEmpty()) {
                rawData.add(getRawRecord(header, rows, rawData.size() + 1));
            }
        } catch (final MalformedURLException e) {
            logger.error("Unable to download from malformed URL {}", url);
            throw new UnrecoverableException("Unable to download file because of malformed url", e);
        } catch (IOException e) {
            logger.error("Unable to create Excel file from data", e);
            throw new UnrecoverableException("Unable to create Excel file from data", e);
        } finally {
            if (inputStream != null) {
                try {
                    inputStream.close();
                } catch (IOException e) {
                    logger.error("Failed to close input stream.");
                }
            }

            File f = new File(wbPath);
            if (f.isFile()) {
                f.delete();
            }
        }

        logger.info("XLS file from {} downloaded and divided into {} parts", url, rawData.size());
        return rawData;
    }

    /**
     * @param header
     *      header row
     * @param rows
     *      data rows
     * @param index
     *      fragment index
     * @return raw record
     */
    private Raw getRawRecord(final List<String> header, final List<List<String>> rows, final int index) {
        return XLSXUtils.getFragmentRawRecord(header, rows, binaryDataUrl, CONTENT_TYPE, index, HSSFWorkbook.class);
    }

    /**
     * The method converts excel number to datetime.
     *
     * @param value
     *      excel number
     * @return datetime
     */
    private static String excelNumberToDateTime(final String value) {
        Double number = Double.valueOf(value);
        Date date = DateUtil.getJavaDate(number);
        return DATETIME_FORMAT.format(date);
    }
}
