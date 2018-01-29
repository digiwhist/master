package eu.digiwhist.worker.cz.parsed;

import eu.digiwhist.dataaccess.dto.parsed.ParsedBudgetItem;
import org.apache.commons.csv.CSVRecord;

/**
 * Parses budget items from FINM201 csv dataset.
 *
 * @author Tomas Mrazek
 */
public final class StatniPokladnaFINM201Handler implements StatniPokladnaHandler {

    @Override
    public ParsedBudgetItem parse(final CSVRecord record) {
        return StatniPokladnaBudgetParserUtils.parseCommonFINMBudgetItem(record)
            .setLevel2Code(record.get(8))
            .setLevel3Code(record.get(9))
            .setValue(record.get(12))
            .setPlannedValue(record.get(10));
    }

    @Override
    public int getIcoColumnIndex() {
        return 4;
    }
}
