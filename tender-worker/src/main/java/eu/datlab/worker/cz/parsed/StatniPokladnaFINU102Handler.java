package eu.datlab.worker.cz.parsed;

import org.apache.commons.csv.CSVRecord;

import eu.datlab.dataaccess.dto.parsed.ParsedBudgetItem;

/**
 * Parses budget items from FINU102 csv dataset.
 *
 * @author Tomas Mrazek
 */
public final class StatniPokladnaFINU102Handler extends BaseStatniPokladnaFINUHandler {
    @Override
    protected ParsedBudgetItem parseDatasetSpecificFields(final ParsedBudgetItem parsed, final CSVRecord record) {
        return parsed
            .setLevel3Code(record.get(6))
            .setLevel2Code(record.get(5))
            .setValue(record.get(10))
            .setPlannedValue(record.get(7));
    }
}
