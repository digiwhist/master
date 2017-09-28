package eu.digiwhist.worker.cz.parsed;

import eu.digiwhist.dataaccess.dto.parsed.ParsedBudgetItem;
import org.apache.commons.csv.CSVRecord;

/**
 * Parses budget items from FINU104 csv dataset.
 *
 * @author Tomas Mrazek
 */
public final class StatniPokladnaFINU104Handler implements StatniPokladnaHandler {

    @Override
    public ParsedBudgetItem parse(final CSVRecord record) {
        return StatniPokladnaBudgetParserUtils.parseCommonFINUBudgetItem(record)
            .setLevel3Code(record.get(7))
            .setLevel2Code(record.get(6))
            .setValue(record.get(9));
    }

    @Override
    public int getIcoColumnIndex() {
        return 4;
    }
}
