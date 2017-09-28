package eu.digiwhist.worker.cz.parsed;

import eu.digiwhist.dataaccess.dto.parsed.ParsedBudgetItem;
import org.apache.commons.csv.CSVRecord;

/**
 * Parses budget items from FINU106 csv dataset.
 *
 * @author Tomas Mrazek
 */
public final class StatniPokladnaFINU106Handler implements StatniPokladnaHandler {

    @Override
    public ParsedBudgetItem parse(final CSVRecord record) {
        return StatniPokladnaBudgetParserUtils.parseCommonFINUBudgetItem(record)
            .setLevel3Code(record.get(6))
            .setValue(record.get(10))
            .setPlannedValue(record.get(7));
    }

    @Override
    public int getIcoColumnIndex() {
        return 4;
    }
}
