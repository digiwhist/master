package eu.datlab.worker.ro.raw;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.raw.Raw;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

/**
 * Downloads XLSX file.
 */
public final class XLSXHandler implements APADownloaderHandler {
    private final Logger logger = LoggerFactory.getLogger(this.getClass().getName());

    private URL binaryDataUrl;
    private static final String CONTENT_TYPE = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet";

    private static final int ROWS_COUNT_LIMIT = 1000;

    /**
     * @param url
     *      url to be downloaded
     * @return list of raw records
     */
    @Override
    public List<Raw> handle(final String url) {
        List<Raw> rawData = new ArrayList<>();

        String wbPath = XLSXUtils.downloadWorkbook(url);
        try {
            binaryDataUrl = new URL(url);

            List<String> header = new ArrayList<>();
            List<List<String>> rows = new ArrayList<>();

            XSSFSAXParser xssf = new XSSFSAXParser((num, data) -> {
                if (num == 1) {
                    header.addAll(data);
                } else {
                    if (rows.size() == ROWS_COUNT_LIMIT) {
                        rawData.add(getRawRecord(header, rows, rawData.size() + 1));
                        rows.clear();
                    } else {
                        rows.add(data);
                    }
                }
            });

            xssf.processFirstSheet(wbPath);

            // save rest of rows
            if (!rows.isEmpty()) {
                rawData.add(getRawRecord(header, rows, rawData.size() + 1));
            }
        } catch (final MalformedURLException e) {
            logger.error("Unable to download from malformed URL {}", url);
            throw new UnrecoverableException("Unable to download file because of malformed url", e);
        } catch (Exception e) {
            logger.error("Unable to parse Excel file", e);
            throw new UnrecoverableException("Unable to parse Excel file", e);
        } finally {
            File f = new File(wbPath);
            if (f.isFile()) {
                f.delete();
            }
        }

        logger.info("XLSX file from {} downloaded and divided into {} parts", url, rawData.size());
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
        return XLSXUtils.getFragmentRawRecord(header, rows, binaryDataUrl, CONTENT_TYPE, index, XSSFWorkbook.class);
    }
}
