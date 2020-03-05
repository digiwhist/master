package eu.datlab.worker.es.parsed;

import eu.datlab.dataaccess.dto.parsed.ParsedBudgetItem;
import eu.datlab.worker.parser.BaseDatlabBudgetItemParser;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.raw.RawData;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.IntStream;

/**
 * Parser for ES budgets.
 */
public final class BudgetExcelParser extends BaseDatlabBudgetItemParser {
    private static final String VERSION = "4";

    @Override
    public List<ParsedBudgetItem> parse(final RawData raw) {
        final InputStream myxls = new ByteArrayInputStream(raw.getSourceBinaryData());
        final Sheet sheet;
        try {
            final HSSFWorkbook wb = new HSSFWorkbook(myxls);
            sheet = wb.getSheetAt(wb.getNumberOfSheets() - 1);
        } catch (IOException e) {
            logger.error("Unable to create Excel file from data", e);
            throw new UnrecoverableException("Unable to create Excel file from data", e);
        }

        
        final Row headerRow = sheet.getRow(6);
        final String year = getCellValue(sheet.getRow(1).getCell(0));
        final int[] dataCells = IntStream.rangeClosed(6, 25).toArray();

        List<ParsedBudgetItem> parsedBudets = new ArrayList<>();
        for (int rowNumber = 7; rowNumber < sheet.getLastRowNum(); rowNumber++) {
            final Row currentRow = sheet.getRow(rowNumber);

            ParsedBody body = new ParsedBody()
                .setName(getCellValue(currentRow.getCell(3)))
                .setAddress(new ParsedAddress().setCountry("ESP"));

            for (int cellNumber : dataCells) {
                ParsedBudgetItem item = new ParsedBudgetItem()
                    .setBody(body)
                    .setYear(year)
                    .setCurrency("EUR")
                    .setValue(getCellValue(currentRow.getCell(cellNumber)));

                String name = getCellValue(headerRow.getCell(cellNumber));

                if (cellNumber == 15 || cellNumber == 25) {
                    item.setLevel1Name(name);
                } else {
                    item.setLevel2Name(name);
                }

                parsedBudets.add(item);
            }
        }

        return parsedBudets;
    }

    @Override
    protected List<ParsedBudgetItem> postProcess(final List<ParsedBudgetItem> parsed, final RawData raw) {
        return parsed;
    }

    @Override
    protected String getVersion() {
        return VERSION;
    }

    /**
     * Get value of cell as string.
     *
     * @param cell cell to get value from
     *
     * @return String or null
     */
    private String getCellValue(final Cell cell) {
        if (cell.getCellType() == CellType.STRING) {
            return cell.getStringCellValue();
        } else if (cell.getCellType() == CellType.NUMERIC) {
            return String.valueOf(cell.getNumericCellValue());
        } else {
            return null;
        }
    }
}
