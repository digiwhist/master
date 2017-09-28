package eu.digiwhist.worker.cz.parsed;

import eu.digiwhist.dataaccess.dto.parsed.ParsedBudgetItem;
import org.apache.commons.csv.CSVRecord;

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
}
