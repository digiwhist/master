package eu.dl.dataaccess.dto.parsed;

// TODO: Auto-generated Javadoc
/**
 * CPV.
 */
public class ParsedCPV {
    /**
     * CPV code of contract subject.
     */
    private String code;

    /**
     * CPV is for main subject.
     */
    private String isMain;

    /**
     * Gets the code.
     *
     * @return the code
     */
    public final String getCode() {
        return code;
    }

    /**
     * Sets the code.
     *
     * @param code
     *            the code
     * @return the parsed CPV
     */
    public final ParsedCPV setCode(final String code) {
        this.code = code;
        return this;
    }

    /**
     * Gets the checks if is main.
     *
     * @return the checks if is main
     */
    public final String getIsMain() {
        return isMain;
    }

    /**
     * Sets the is main.
     *
     * @param isMain
     *            the is main
     * @return the parsed CPV
     */
    public final ParsedCPV setIsMain(final String isMain) {
        this.isMain = isMain;
        return this;
    }
}
