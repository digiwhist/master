package eu.digiwhist.worker.eu.parsed;

/**
 * Form version.
 *
 * @author Tomas Mrazek
 */
enum TedFormVersionType {
    /**
     * Form version prior R2.0.8.S01.E01.
     */
    PRIOR_R209("R2.0.8.S01.E01"),
    /**
     * Form version R2.0.9.S01.E01.
     */
    R209("R2.0.9.S01.E01");
    /**
     * Comparing method is older.
     */
    public static final int IS_OLDER = 1;
    /**
     * Comparing method is younger.
     */
    public static final int IS_YOUNGER = 2;
    /**
     * Comparing method is equal.
     */
    public static final int IS_EQUAL = 4;
    /**
     * Version string.
     */
    private final String versionString;

    /**
     * Constructor with versionString initialization.
     *
     * @param versionString
     *      version string
     */
    TedFormVersionType(final String versionString) {
        this.versionString = versionString;
    }

    /**
     * @return version string
     */
    public String getVersionString() {
        return versionString;
    }

    /**
     * Checks whether this version meets the given condition.
     *
     * @param version
     *      version for comparsion
     * @param comparsionMethod
     *      comparsion method. Supported method are IS_OLDER, IS_YOUNGER, IS_EQUAL or their combination.
     *      - eg. IS_OLDER | IS_EQUAL means "Check this version whether is older or equal to the given {@code version}".
     * @return decision if this version meets the condition
     */
    public boolean is(final String version, final int comparsionMethod) {
        final int value = versionString.compareToIgnoreCase(version);

        return
            ((comparsionMethod & IS_OLDER) > 0 && value < 0)
            || ((comparsionMethod & IS_YOUNGER) > 0 && value > 0)
            || ((comparsionMethod & IS_EQUAL) > 0 && value == 0);
    }

    /**
     * Checks whether this version meets the given condition.
     *
     * @param version
     *      version for comparsion
     * @param comparingMethod
     *      comparsion method. Supported method are IS_OLDER, IS_YOUNGER, IS_EQUAL or their combination.
     *      - eg. IS_OLDER | IS_EQUAL means "Check this version whether is older or equal to the given {@code version}".
     * @return decision if this version meets the condition
     */
    public boolean is(final TedFormVersionType version, final int comparingMethod) {
        return is(version.getVersionString(), comparingMethod);
    }
}
