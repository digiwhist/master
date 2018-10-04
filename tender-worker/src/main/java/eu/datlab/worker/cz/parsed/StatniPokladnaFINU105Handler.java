package eu.datlab.worker.cz.parsed;

import org.apache.commons.csv.CSVRecord;

import eu.datlab.dataaccess.dto.parsed.ParsedBudgetItem;

/**
 * Parses budget items from FINU105 csv dataset.
 *
 * @author Tomas Mrazek
 */
public final class StatniPokladnaFINU105Handler extends BaseStatniPokladnaFINUHandler {
    @Override
    protected ParsedBudgetItem parseDatasetSpecificFields(final ParsedBudgetItem parsed, final CSVRecord record) {
        return parsed
            .setLevel3Code(record.get(6))
            .setValue(record.get(7));
    }
}
