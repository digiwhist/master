package eu.digiwhist.worker.cz.parsed;

import eu.digiwhist.dataaccess.dto.codetables.BudgetItemReportType;
import eu.digiwhist.dataaccess.dto.parsed.ParsedBudgetItem;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.codetables.CountryCode;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import org.apache.commons.csv.CSVRecord;

/**
 * Common and utility functions for StatniPokladnaBudget parsers.
 */
public final class StatniPokladnaBudgetParserUtils {
    /**
     * Suppress default constructor for noninstantiability.
     */
    private StatniPokladnaBudgetParserUtils() {
        throw new AssertionError();
    }

    /**
     * Parses common badget item data from FINU dataset.
     *
     * @param csvRecord
     *      csv record that includes budget item data
     * @return parsed budget item initialized with common data
     */
    public static ParsedBudgetItem parseCommonFINUBudgetItem(final CSVRecord csvRecord) {
        return parseCommonBudgetItem(csvRecord)
            .setReport(BudgetItemReportType.BUDGET_EXECUTION.name())
            .setYear(parseYear(csvRecord.get(2)))
            .setBody(new ParsedBody()
                .addBodyId(parseICO(csvRecord.get(4))))
            .setLevel1Code(csvRecord.get(5))
            .setCountry(CountryCode.CZ.name());
    }

    /**
     * Parses common badget item data from FINM dataset.
     *
     * @param csvRecord
     *      csv record that includes budget item data
     * @return parsed budget item initialized with common data
     */
    public static ParsedBudgetItem parseCommonFINMBudgetItem(final CSVRecord csvRecord) {
        return parseCommonBudgetItem(csvRecord)
            .setReport(BudgetItemReportType.BUDGET_EXECUTION.name())
            .setYear(parseYear(csvRecord.get(2)))
            .setBody(new ParsedBody()
                .addBodyId(parseICO(csvRecord.get(4)))
                .setAddress(new ParsedAddress()
                    .addNuts(csvRecord.get(6))))
            .setCountry(CountryCode.CZ.name());
    }

    /**
     * Parses common badget item data.
     *
     * @param csvRecord
     *      csv record that includes budget item data
     * @return parsed budget item initialized with common data
     */
    public static ParsedBudgetItem parseCommonBudgetItem(final CSVRecord csvRecord) {
        return new ParsedBudgetItem().setCountry(CountryCode.CZ.name());
    }

    /**
     * Creates BodyIdentifier for the given {@code ico}. Sets the scope to {@code BodyIdentifier.Scope.CZ} and the
     * type to {@code BodyIdentifier.Type.ORGANIZATION_ID}
     *
     * @param ico
     *      ICO to set
     * @return body identifier
     */
    public static BodyIdentifier parseICO(final String ico) {
        return new BodyIdentifier()
            .setId(ico)
            .setScope(BodyIdentifier.Scope.CZ)
            .setType(BodyIdentifier.Type.ORGANIZATION_ID);
    }

    /**
     * Parses year as first four characters (supposes that these characters are numbers) from the {@code input} string.
     *
     * @param input
     *      input string
     * @return year or null if the input string is null or shorter than four characters
     */
    public static String parseYear(final String input) {
        if (input == null || input.length() < 4) {
            return null;
        }

        return input.substring(0, 4);
    }

    
}
