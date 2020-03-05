package eu.datlab.worker.ro.raw;

import org.apache.poi.hssf.eventusermodel.HSSFListener;
import org.apache.poi.hssf.record.BOFRecord;
import org.apache.poi.hssf.record.BoundSheetRecord;
import org.apache.poi.hssf.record.LabelSSTRecord;
import org.apache.poi.hssf.record.NumberRecord;
import org.apache.poi.hssf.record.Record;
import org.apache.poi.hssf.record.RowRecord;
import org.apache.poi.hssf.record.SSTRecord;

import java.util.ArrayList;
import java.util.List;
import java.util.function.BiConsumer;

/**
 * XLS parser.
 */
public final class HSSFParser implements HSSFListener {
    private final BiConsumer<Integer, List<String>> rowHandler;

    private SSTRecord sstrec;

    private List<String> currentRow = new ArrayList<>();
    private Integer currentRowNumber = 0;


    /**
     * Constructor with row handler initialization.
     *
     * @param handler
     *      row handler to be set, accepts two parameters, row number and row data
     */
    public HSSFParser(final BiConsumer<Integer, List<String>> handler) {
        this.rowHandler = handler;
    }

    /**
     * Default constructor without row handler.
     */
    public HSSFParser() {
        this.rowHandler = null;
    }

    @Override
    public void processRecord(final Record record) {
        switch (record.getSid()) {
            // the BOFRecord can represent either the beginning of a sheet or the workbook
            case BOFRecord.sid:
            case BoundSheetRecord.sid:
            case RowRecord.sid:
                break;
            case NumberRecord.sid:
                NumberRecord numrec = (NumberRecord) record;
                updateRow(numrec.getRow(), String.valueOf(numrec.getValue()));
                break;
            // SSTRecords store a array of unique strings used in Excel.
            case SSTRecord.sid:
                sstrec = (SSTRecord) record;
                break;
            case LabelSSTRecord.sid:
                LabelSSTRecord lrec = (LabelSSTRecord) record;
                updateRow(lrec.getRow(), sstrec.getString(lrec.getSSTIndex()).toString());
                break;
            default:
                break;
        }
    }

    /**
     * @param rowNumber
     *      number of processed row
     * @param value
     *      cell value
     */
    private void updateRow(final int rowNumber, final String value) {
        if (currentRowNumber != rowNumber) {
            // handler bellow assumes 1-based row number, therefore it's possible to increment row number before the handler
            // calling.
            currentRowNumber++;
            rowHandler.accept(currentRowNumber, currentRow);
            currentRow = new ArrayList<>();
        }

        currentRow.add(value);
    }
}
