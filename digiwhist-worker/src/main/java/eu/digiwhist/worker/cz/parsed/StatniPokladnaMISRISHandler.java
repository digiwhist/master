package eu.digiwhist.worker.cz.parsed;

import eu.digiwhist.dataaccess.dto.codetables.BudgetItemReportType;
import eu.digiwhist.dataaccess.dto.parsed.ParsedBudgetItem;
import static eu.digiwhist.worker.cz.parsed.StatniPokladnaBudgetParserUtils.parseYear;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import org.apache.commons.csv.CSVRecord;

/**
 * Parses budget items from MIS-RIS csv dataset.
 *
 * @author Tomas Mrazek
 */
public final class StatniPokladnaMISRISHandler implements StatniPokladnaHandler {

    @Override
    public ParsedBudgetItem parse(final CSVRecord record) {
        return StatniPokladnaBudgetParserUtils.parseCommonBudgetItem(record)
            .setReport(BudgetItemReportType.BUDGET_EXECUTION.name())
            .setYear(parseYear(record.get(0)))
            .setLevel3Code(record.get(5))
            .setLevel2Code(record.get(6))
            .setValue(record.get(15))
            .setPlannedValue(record.get(11))
            .setBody(new ParsedBody()
                .addBodyId(StatniPokladnaBudgetParserUtils.parseICO(record.get(getIcoColumnIndex()))))
            .setLevel1Code(record.get(3));
    }

    @Override
    public int getIcoColumnIndex() {
        return 2;
    }
}
