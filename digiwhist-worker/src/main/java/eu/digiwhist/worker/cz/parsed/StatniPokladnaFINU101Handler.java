package eu.digiwhist.worker.cz.parsed;

import eu.digiwhist.dataaccess.dto.parsed.ParsedBudgetItem;
import org.apache.commons.csv.CSVRecord;


/**
 * Parses budget items from FINU101 csv dataset.
 *
 * @author Tomas Mrazek
 */
public final class StatniPokladnaFINU101Handler implements StatniPokladnaHandler {

    @Override
    public ParsedBudgetItem parse(final CSVRecord record) {
        return StatniPokladnaBudgetParserUtils.parseCommonFINUBudgetItem(record)
            .setLevel3Code(record.get(8))
            .setLevel2Code(record.get(7))
            .setValue(record.get(12))
            .setPlannedValue(record.get(9));
    }

    @Override
    public int getIcoColumnIndex() {
        return 4;
    }
}
