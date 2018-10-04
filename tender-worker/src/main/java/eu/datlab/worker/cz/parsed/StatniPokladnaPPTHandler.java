package eu.datlab.worker.cz.parsed;

import eu.datlab.dataaccess.dto.codetables.BudgetItemReportType;
import eu.datlab.dataaccess.dto.parsed.ParsedBudgetItem;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import org.apache.commons.csv.CSVRecord;

import static eu.datlab.worker.cz.parsed.StatniPokladnaBudgetParserUtils.parseYear;

/**
 * Parses budget items from PPT csv dataset.
 *
 * @author Tomas Mrazek
 */
public final class StatniPokladnaPPTHandler implements StatniPokladnaHandler {
    @Override
    public ParsedBudgetItem parse(final CSVRecord record) {
        return StatniPokladnaBudgetParserUtils.parseCommonBudgetItem(record)
            .setYear(parseYear(record.get(2)))
            .setBody(new ParsedBody()
                .addBodyId(StatniPokladnaBudgetParserUtils.parseICO(record.get(getIcoColumnIndex())))
                .setAddress(new ParsedAddress()
                    .addNuts(record.get(7))))
            .setLevel1Code(record.get(5))
            .setLevel3Code(record.get(8))
            .setValue(record.get(9));
    }

    @Override
    public int getIcoColumnIndex() {
        return 4;
    }

    @Override
    public BudgetItemReportType getReportType() {
        return BudgetItemReportType.CASH_FLOW;
    }
}
