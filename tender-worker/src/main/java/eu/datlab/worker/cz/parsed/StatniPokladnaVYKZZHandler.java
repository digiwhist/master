package eu.datlab.worker.cz.parsed;

import eu.datlab.dataaccess.dto.codetables.BudgetItemReportType;
import eu.datlab.dataaccess.dto.parsed.ParsedBudgetItem;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBody;

import static eu.datlab.worker.cz.parsed.StatniPokladnaBudgetParserUtils.parseYear;

import java.math.BigDecimal;
import org.apache.commons.csv.CSVRecord;

/**
 * Parses budget items from VYKZZ csv dataset.
 *
 * @author Tomas Mrazek
 */
public final class StatniPokladnaVYKZZHandler implements StatniPokladnaHandler {

    @Override
    public ParsedBudgetItem parse(final CSVRecord record) {
        return StatniPokladnaBudgetParserUtils.parseCommonBudgetItem(record)
            .setYear(parseYear(record.get(2)))
            .setBody(new ParsedBody()
                .addBodyId(StatniPokladnaBudgetParserUtils.parseICO(record.get(getIcoColumnIndex())))
                .setAddress(new ParsedAddress()
                    .addNuts(record.get(7))))
            .setLevel1Code(record.get(5))
            .setLevel2Code(record.get(9))
            .setLevel3Code(record.get(8))
            .setValue(parseValue(record.get(10)).add(parseValue(record.get(11))).toString());
    }

    @Override
    public int getIcoColumnIndex() {
        return 4;
    }

    @Override
    public BudgetItemReportType getReportType() {
        return BudgetItemReportType.PROFIT_AND_LOSS;
    }

    /**
     * @param value
     *      string value
     * @return value as BigDecimal or zero
     */
    private static BigDecimal parseValue(final String value) {
        if (value == null || value.isEmpty()) {
            return BigDecimal.ZERO;
        }

        try {
            return new BigDecimal(value.trim().replace(" ", ""));
        } catch (final Exception ex) {
            return BigDecimal.ZERO;
        }
    }
}
