package eu.dl.worker.clean.utils;

/**
 * Type of url scheme.
 *
 * @author Tomas Mrazek
 */
public enum URLSchemeType {
    /**
     * HTTP.
     */
    HTTP(true),
    /**
     * Secure HTTP.
     */
    HTTPS(true),
    /**
     * FILE.
     */
    FILE(true),
    /**
     * FTP.
     */
    FTP(true),
    /**
     * Secure FTP.
     */
    FTPS(true);

    private final boolean addsSlashes;

    /**
     * Url scheme initialization.
     *
     * @param addSlashes
     *      slashess "//" are required by some schemes and not required by some others
     */
    URLSchemeType(final boolean addSlashes) {
        this.addsSlashes = addSlashes;
    }

    /**
     * Returns url scheme string. Appends "://" to the scheme name in lowercase.
     *
     * @return scheme string
     */
    public String getScheme() {
        final StringBuilder scheme = new StringBuilder(this.name().toLowerCase()).append(":");
        if (addsSlashes) {
            scheme.append("//");
        }

        return scheme.toString();
    }
}
