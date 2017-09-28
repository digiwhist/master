package eu.digiwhist.worker.cz.parsed;

import eu.digiwhist.dataaccess.dto.parsed.ParsedBudgetItem;
import org.apache.commons.csv.CSVRecord;

/**
 * Parses budget items from FINU102 csv dataset.
 *
 * @author Tomas Mrazek
 */
public final class StatniPokladnaFINU102Handler implements StatniPokladnaHandler {

    @Override
    public ParsedBudgetItem parse(final CSVRecord record) {
        return StatniPokladnaBudgetParserUtils.parseCommonFINUBudgetItem(record)
            .setLevel3Code(record.get(6))
            .setLevel2Code(record.get(5))
            .setValue(record.get(10))
            .setPlannedValue(record.get(7));
    }

    @Override
    public int getIcoColumnIndex() {
        return 4;
    }
}
