package eu.datlab.worker.cz.parsed;

import org.apache.commons.csv.CSVRecord;

import eu.datlab.dataaccess.dto.parsed.ParsedBudgetItem;

/**
 * Parses budget items from FINM202 csv dataset.
 *
 * @author Tomas Mrazek
 */
public final class StatniPokladnaFINM202Handler extends BaseStatniPokladnaFINMHandler {
    @Override
    protected ParsedBudgetItem parseDatasetSpecificFields(final ParsedBudgetItem parsed, final CSVRecord record) {
        return parsed
            .setLevel3Code(record.get(7))
            .setValue(record.get(10))
            .setPlannedValue(record.get(8));
    }
}
