package eu.datlab.worker.ro.parsed;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.raw.RawData;
import org.apache.poi.hssf.usermodel.HSSFCell;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.slf4j.Logger;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;

/**
 * Parser handler for RO source in excel format.
 */
public final class APATenderExcelHandler {

    private static int minHeaderSizeCFT = 16;
    private static int minHeaderSizeContracte = 38;
    private static int minHeaderSizeDirectContract = 20;

    /**
     * Private constructor for noninstantiabilit.
     */
    private APATenderExcelHandler(){

    }

    /**
     * Parses the given raw data.
     * @param raw raw data
     * @param logger logger
     * @return list of parsed tenders
     */
    public static List<ParsedTender> parse(final RawData raw, final Logger logger) {

        final InputStream myxls = new ByteArrayInputStream(raw.getSourceBinaryData());
        final Sheet sheet;
        try {
            if(raw.getSourceUrl().toString().endsWith("xls")) {
                sheet = (new HSSFWorkbook(myxls)).getSheetAt(0);
            } else {  // Office 2007+ XML (ends with xlsx)
                sheet = (new XSSFWorkbook(myxls)).getSheetAt(0);
            }
        } catch (IOException e) {
            logger.error("Unable to create Excel file from data", e);
            throw new UnrecoverableException("Unable to create Excel file from data", e);
        }
        final int headerNumberOfValues = sheet.getRow(0).getLastCellNum();

        if (headerNumberOfValues >= minHeaderSizeContracte) {
            return APATenderExcelContracteHandler.parse(sheet, logger);
        } else if(headerNumberOfValues >= minHeaderSizeDirectContract){
            return APATenderExcelDirectContractHandler.parse(sheet, logger);
        } else {
            return APATenderExcelCFTHandler.parse(sheet, logger);
        }
    }

    /**
     * Get value of cell as string.
     *
     * @param cell cell to get value from
     * @return String or null
     */
    public static String getCellValue(final Cell cell) {
        if (cell.getCellType() == HSSFCell.CELL_TYPE_STRING) {
            return cell.getStringCellValue();
        } else if (cell.getCellType() == HSSFCell.CELL_TYPE_NUMERIC) {
            return String.valueOf(cell.getNumericCellValue());
        } else {
            return null;
        }
    }
}
