package eu.datlab.worker.cz.parsed;

import eu.datlab.dataaccess.dto.codetables.BudgetItemReportType;
import org.apache.commons.csv.CSVRecord;

import eu.datlab.dataaccess.dto.parsed.ParsedBudgetItem;

/**
 * Budget handler interface.
 *
 * @author Tomas Mrazek
 */
public interface StatniPokladnaHandler {

    /**
     * Parses dataset specific attributes of the budget item.
     *
     * @param record
     *         budget item record
     * @return budget item
     */
    ParsedBudgetItem parse(CSVRecord record);

    /**
     * @return index of the column that includes ICO
     */
    int getIcoColumnIndex();

    /**
     * @return report type
     */
    BudgetItemReportType getReportType();
}
