package eu.digiwhist.worker.cz.parsed;

import eu.digiwhist.dataaccess.dto.codetables.BudgetItemReportType;
import eu.digiwhist.dataaccess.dto.parsed.ParsedBudgetItem;
import static eu.digiwhist.worker.cz.parsed.StatniPokladnaBudgetParserUtils.parseYear;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import org.apache.commons.csv.CSVRecord;

/**
 * Parses budget items from MIS-RIS-ZU csv dataset.
 *
 * @author Tomas Mrazek
 */
public final class StatniPokladnaMISRISZUHandler implements StatniPokladnaHandler {

    @Override
    public ParsedBudgetItem parse(final CSVRecord record) {
        return StatniPokladnaBudgetParserUtils.parseCommonBudgetItem(record)
            .setReport(BudgetItemReportType.BUDGET_EXECUTION.name())
            .setYear(parseYear(record.get(0)))
            .setLevel1Code(record.get(3))
            .setLevel3Code(record.get(10))
            .setValue(record.get(11))
            .setPlannedValue(record.get(7))
            .setBody(new ParsedBody()
                .addBodyId(StatniPokladnaBudgetParserUtils.parseICO(record.get(getIcoColumnIndex()))));
    }

    @Override
    public int getIcoColumnIndex() {
        return 2;
    }
}
