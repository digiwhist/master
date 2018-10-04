package eu.datlab.worker.cz.parsed;

import org.apache.commons.csv.CSVRecord;

import eu.datlab.dataaccess.dto.parsed.ParsedBudgetItem;


/**
 * Parses budget items from FINU101 csv dataset.
 *
 * @author Tomas Mrazek
 */
public final class StatniPokladnaFINU101Handler extends BaseStatniPokladnaFINUHandler {
    @Override
    protected ParsedBudgetItem parseDatasetSpecificFields(final ParsedBudgetItem parsed, final CSVRecord record) {
        return parsed
            .setLevel3Code(record.get(8))
            .setLevel2Code(record.get(7))
            .setValue(record.get(12))
            .setPlannedValue(record.get(9));
    }
}
