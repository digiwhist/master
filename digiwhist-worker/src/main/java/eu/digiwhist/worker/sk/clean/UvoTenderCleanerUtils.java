package eu.digiwhist.worker.sk.clean;

/**
 * Utils for SK UVO cleaner.
 *
 * @author Michal Riha
 */
public final class UvoTenderCleanerUtils {

    /**
     * Private constructor to make this class static.
     */
    private UvoTenderCleanerUtils() {
    }

    /**
     * Clean SK UVO date specific glitches.
     *
     * @param date
     *         date to clean
     *
     * @return date partially cleaned
     */
    public static String cleanDatesAndTimes(final String date) {
        if (date == null) {
            return null;
        } else {
            return date.replaceAll("h.", "").replace("Dátum:", "").replace("Čas:", "").replaceAll("  ", " ");
        }
    }

    /**
     * Clean SK UVO price specific glitches.
     *
     * @param price
     *         price to clean
     *
     * @return uvo partially cleaned
     */
    public static String cleanUvoPrice(final String price) {
        if (price == null) {
            return null;
        } else {
            return price.replace("EUR", "").replaceAll(" ", "").trim();
        }
    }
}
