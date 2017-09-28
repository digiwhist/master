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
     * Removes dots at the end of input string. The string is trimmed to remove spaces and get the dots at the end.
     *
     * @param string
     *         string to remove dots
     *
     * @return trimmed input string without dots and spaces at the end of input string
     */
    static String removeDotsAtTheEnd(final String string) {
        if (string == null) {
            return null;
        }

        String stringToModify = string;

        boolean wasStringModified;
        // while loop is necessary, because the string can be " , à 17 heures . ."
        do {
            if (stringToModify.endsWith(" ")) {
                stringToModify = stringToModify.trim();
                wasStringModified = true;
            } else if (stringToModify.endsWith(".")) {
                stringToModify = stringToModify.substring(0, stringToModify.length() - 1);
                wasStringModified = true;
            } else {
                wasStringModified = false;
            }
        } while (wasStringModified);

        return stringToModify;
    }

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
