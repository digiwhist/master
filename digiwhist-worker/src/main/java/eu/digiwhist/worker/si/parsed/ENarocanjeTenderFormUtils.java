package eu.digiwhist.worker.si.parsed;

/**
 * Utilities for ENarocanje.
 *
 * @author Marek Mikes
 */
final class ENarocanjeTenderFormUtils {
    /**
     * Private constructor to make this class static.
     */
    private ENarocanjeTenderFormUtils() {
    }

    static final String IS_EU_FUND_TITLE =
            "Naročilo se nanaša na projekt in/ali program, ki se financira s sredstvi EU:";

    /**
     * Decides whether input string represents "yes" or "no" in Slovenian language.
     *
     * @param yesOrNoString
     *         string which represents "yes" or "no" in Slovenian language
     *
     * @return true when input string represents "yes" in Slovenian language;
     *         false when input string represents "no" in Slovenian language;
     *         otherwise null
     */
    static Boolean meansYes(final String yesOrNoString) {
        if (yesOrNoString == null) {
            return null;
        }

        String yesOrNoStringCleaned = yesOrNoString;

        if (yesOrNoStringCleaned.endsWith(".")) {
            yesOrNoStringCleaned = yesOrNoStringCleaned.substring(0, yesOrNoStringCleaned.length() - 1);
        }

        if (yesOrNoStringCleaned.isEmpty()) {
            return null;
        }

        switch (yesOrNoStringCleaned.trim()) {
            case "Da":
            case "da":
                return true;
            case "Ne":
            case "ne":
                return false;
            default:
                assert false;
                return null;
        }
    }

}
