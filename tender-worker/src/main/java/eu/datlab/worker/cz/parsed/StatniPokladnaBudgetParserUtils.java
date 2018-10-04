package eu.datlab.worker.cz.parsed;

import eu.datlab.dataaccess.dto.parsed.ParsedBudgetItem;
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
     * @param icoColumnIndex
     *      index if column that includes ico (0 based)
     * @return parsed budget item initialized with common data
     */
    public static ParsedBudgetItem parseCommonFINUBudgetItem(final CSVRecord csvRecord, final int icoColumnIndex) {
        return parseCommonBudgetItem(csvRecord)
            .setYear(parseYear(csvRecord.get(2)))
            .setBody(new ParsedBody()
                .addBodyId(parseICO(csvRecord.get(icoColumnIndex))))
            .setLevel1Code(csvRecord.get(5));
    }

    /**
     * Parses common badget item data from FINM dataset.
     *
     * @param csvRecord
     *      csv record that includes budget item data
     * @param icoColumnIndex
     *      index if column that includes ico (0 based)
     * @return parsed budget item initialized with common data
     */
    public static ParsedBudgetItem parseCommonFINMBudgetItem(final CSVRecord csvRecord, final int icoColumnIndex) {
        return parseCommonBudgetItem(csvRecord)
            .setYear(parseYear(csvRecord.get(2)))
            .setBody(new ParsedBody()
                .addBodyId(parseICO(csvRecord.get(icoColumnIndex)))
                .setAddress(new ParsedAddress()
                    .addNuts(csvRecord.get(6))));
    }

    /**
     * Parses common badget item data.
     *
     * @param csvRecord
     *      csv record that includes budget item data
     * @return parsed budget item initialized with common data
     */
    public static ParsedBudgetItem parseCommonBudgetItem(final CSVRecord csvRecord) {
        return new ParsedBudgetItem()
            .setCountry(CountryCode.CZ.name())
            .setReportSubType(csvRecord.get(1));
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
