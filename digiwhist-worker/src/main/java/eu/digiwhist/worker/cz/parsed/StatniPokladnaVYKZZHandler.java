package eu.digiwhist.worker.cz.parsed;

import eu.digiwhist.dataaccess.dto.codetables.BudgetItemReportType;
import eu.digiwhist.dataaccess.dto.parsed.ParsedBudgetItem;
import static eu.digiwhist.worker.cz.parsed.StatniPokladnaBudgetParserUtils.parseYear;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.util.Locale;
import org.apache.commons.csv.CSVRecord;

/**
 * Parses budget items from VYKZZ csv dataset.
 *
 * @author Tomas Mrazek
 */
public final class StatniPokladnaVYKZZHandler implements StatniPokladnaHandler {

    private static final Locale LOCALE = new Locale("en");

    private static final NumberFormat NUMBER_FORMAT;
    static {
        DecimalFormatSymbols formatSymbols = new DecimalFormatSymbols(LOCALE);
        formatSymbols.setDecimalSeparator('.');
        formatSymbols.setGroupingSeparator(' ');
        NUMBER_FORMAT = new DecimalFormat("#,##0.###", formatSymbols);
    }

    @Override
    public ParsedBudgetItem parse(final CSVRecord record) {
        return StatniPokladnaBudgetParserUtils.parseCommonBudgetItem(record)
            .setReport(BudgetItemReportType.PROFIT_AND_LOSS.name())
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
