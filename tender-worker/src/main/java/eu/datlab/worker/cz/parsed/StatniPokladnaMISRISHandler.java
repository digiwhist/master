package eu.datlab.worker.cz.parsed;

import eu.datlab.dataaccess.dto.codetables.BudgetItemReportType;
import eu.datlab.dataaccess.dto.parsed.ParsedBudgetItem;
import eu.dl.dataaccess.dto.parsed.ParsedBody;

import static eu.datlab.worker.cz.parsed.StatniPokladnaBudgetParserUtils.parseYear;

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
            .setYear(parseYear(record.get(0)))
            .setLevel1Code(record.get(3))
            .setLevel2Code(record.get(6))
            .setLevel3Code(record.get(5))
            .setValue(record.get(15))
            .setPlannedValue(record.get(11))
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
