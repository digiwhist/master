package eu.digiwhist.worker.cz.parsed;

import eu.digiwhist.dataaccess.dto.parsed.ParsedBudgetItem;
import org.apache.commons.csv.CSVRecord;

/**
 * Parses budget items from FINM202 csv dataset.
 *
 * @author Tomas Mrazek
 */
public final class StatniPokladnaFINM207Handler implements StatniPokladnaHandler {

    @Override
    public ParsedBudgetItem parse(final CSVRecord record) {
        return StatniPokladnaBudgetParserUtils.parseCommonFINMBudgetItem(record)
            .setLevel3Code(record.get(9))
            .setValue(record.get(10));
    }

    @Override
    public int getIcoColumnIndex() {
        return 4;
    }
}
