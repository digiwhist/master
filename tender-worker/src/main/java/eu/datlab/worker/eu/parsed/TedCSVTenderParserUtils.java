package eu.datlab.worker.eu.parsed;

import java.util.Arrays;
import java.util.List;

import org.apache.commons.csv.CSVRecord;

import eu.dl.dataaccess.dto.parsed.ParsedPrice;

/**
 * Class provides set of functions for parsing typical  .
 *
 * @author Tomas Mrazek
 */
public final class TedCSVTenderParserUtils {
    
    /**
     * Suppress default constructor for noninstantiability.
     */
    private TedCSVTenderParserUtils() {
        throw new AssertionError();
    }
    
    /**
     * Parses Boolean from the CSV record's column value with the given name.
     *
     * @param record
     *      CSV record
     * @param name
     *      column name
     * @return "true" only and only if column value is equal to 1 or Y, otherwise "false".
     */
    public static String parseBoolean(final CSVRecord record, final String name) {
        return valueToBooleanString(record.get(name));
    }

    /**
     * Converts the given string {@code value} to boolean string.
     *
     * @param value
     *      string value
     * @return "true" only and only if column value is equal to 1 or Y, otherwise "false".
     */
    private static String valueToBooleanString(final String value) {
        return String.valueOf(value != null && value.matches("1|Y"));
    }

    /**
     * Parses price from the CSV record. This price is estimatedPrice for Contract notice and finalPrice for Contract
     * award.
     *
     * @param record
     *      CSV record
     * @return price
     */
    public static ParsedPrice parseTenderPrice(final CSVRecord record) {
        return parsePrice(record, Arrays.asList(
            "value_euro_fin_2",     // generally same as VALUE_EURO_FIN_1 but may includes human corrections
            "value_euro_fin_1",     // if VALUE_EURO missing
            "value_euro"
        ));
    }

    /**
     * Parses price from the CSV record's columns. First non-null column value from the {@code columns} list is
     * returned (column order affects the result!).
     *
     * @param record
     *      CSV record
     * @param columns
     *      list of columns
     * @return price or null
     */
    public static ParsedPrice parsePrice(final CSVRecord record, final List<String> columns) {
        if (columns == null || columns.isEmpty()) {
            return null;
        }

        String price = null;
        for (String c : columns) {
            try {
                price = record.get(c);                
            } catch (IllegalArgumentException e) {
                // some columns may be undefined
                continue;
            }
            
            if (price != null) {
                break;
            }
        }

        return price == null ? null : new ParsedPrice().setNetAmount(price).setCurrency("EUR");
    }
}
