package eu.digiwhist.worker.fr.parsed;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Parser utilities for France.
 *
 * @author Marek Mikes
 */
final class BOAMPTenderParserUtils {
    /**
     * Private constructor to make this class static.
     */
    private BOAMPTenderParserUtils() {}

    /**
     * Gets number at the beginning of string.
     * It can be used to get just percent value without '%' character (possible strings are "10 %", "61 %.",
     * "4 points %", "60 ", " 60%.", "60", " 33,33 %.", " 33.33 %.", ..).
     *
     * @param string
     *         string where number is at the beginning
     *
     * @return the number which starts at the beginning of string
     */
    static String getNumberAtTheBeginning(final String string) {
        if (string == null) {
            return null;
        }

        final String stringWithoutLeftSpaces = string.replaceAll("^\\s+", "");

        // at first, try to get natural number
        Pattern r = Pattern.compile("(^\\d{1,3})( |%|$)");
        Matcher m = r.matcher(stringWithoutLeftSpaces);
        if (m.find()) {
            return m.group(1);
        } else {
            // try to get real number - something like "33,33 %." or "33.33 %."
            r = Pattern.compile("(^\\d{1,2}[,.]\\d{1,2})( |%|$)");
            m = r.matcher(stringWithoutLeftSpaces);
            return m.find() ? m.group(1) : null;
        }
    }
}
