package eu.datlab.worker.cz.parsed;

import eu.datlab.dataaccess.dto.codetables.BudgetItemReportType;
import eu.datlab.dataaccess.dto.parsed.ParsedBudgetItem;
import eu.dl.dataaccess.dto.parsed.ParsedBody;

import static eu.datlab.worker.cz.parsed.StatniPokladnaBudgetParserUtils.parseYear;

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

    @Override
    public BudgetItemReportType getReportType() {
        return BudgetItemReportType.BUDGET_EXECUTION;
    }
}
