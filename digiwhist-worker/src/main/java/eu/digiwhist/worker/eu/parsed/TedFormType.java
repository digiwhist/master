package eu.digiwhist.worker.eu.parsed;

/**
 * TED form type enumeration.
 *
 * @author Tomas Mrazek
 */
public enum TedFormType {
    /** Contract notice form. */
    CONTRACT_NOTICE(2),
    /** Contract award notice form. */
    CONTRACT_AWARD_NOTICE(3),
    /** Contract notice form (utilities). */
    CONTRACT_NOTICE_UTILITIES(5),
    /** Contract award notice form (utilities). */
    CONTRACT_AWARD_NOTICE_UTILITIES(6),
    /** Correction. */
    CORRECTION(14);
    
    /** Number code of the form type. */
    private final int code;

    /**
     * Initializes TED form type with its number code.
     *
     * @param code
     *      number code of the form type
     */
    TedFormType(final int code) {
        this.code = code;
    }

    /**
     * @return form type code
     */
    public int getCode() {
        return this.code;
    }

    /**
     * Returns for type for given number code.
     *
     * @param code
     *      form type number code
     * @return form type
     */
    public static TedFormType getFormTypeFromCode(final int code) {
        for (TedFormType type : TedFormType.values()) {
            if (type.getCode() == code) {
                return type;
            }
        }

        return null;
    }

    /**
     * Returns form type for given string code. Only number characters are parsed from given string code and for this
     * parsed integer value is form type returned.
     *
     * @param code
     *      form type number code
     * @return form type
     */
    public static TedFormType getFormTypeFromCode(final String code) {
        return getFormTypeFromCode(Integer.parseInt(code.replaceAll("[^\\d]", ""), 10));
    }
}
