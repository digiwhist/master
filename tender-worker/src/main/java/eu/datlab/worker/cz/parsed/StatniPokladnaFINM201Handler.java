package eu.datlab.worker.cz.parsed;

import org.apache.commons.csv.CSVRecord;

import eu.datlab.dataaccess.dto.parsed.ParsedBudgetItem;

/**
 * Parses budget items from FINM201 csv dataset.
 *
 * @author Tomas Mrazek
 */
public final class StatniPokladnaFINM201Handler extends BaseStatniPokladnaFINMHandler {
    @Override
    protected ParsedBudgetItem parseDatasetSpecificFields(final ParsedBudgetItem parsed, final CSVRecord record) {
        return parsed
            .setLevel2Code(record.get(8))
            .setLevel3Code(record.get(9))
            .setValue(record.get(12))
            .setPlannedValue(record.get(10));
    }
}
