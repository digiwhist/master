package eu.dl.dataaccess.dto.codetables;

import com.fasterxml.jackson.annotation.JsonValue;

/**
 * OCDS procedure method enumeration.
 *
 * @author Tomas Mrazek
 */
public enum OCDSProcedureMethod {
    /**
     * Selective.
     */
    SELECTIVE,
    /**
     * Direct.
     */
    DIRECT,
    /**
     * Limited.
     */
    LIMITED,
    /**
     * Open.
     */
    OPEN;

    @Override
    @JsonValue
    public String toString() {
        return OCDSEnumUtils.ocdsCodelistJsonValue(this);
    }

    /**
     * Return OCDS procedure method for the given TenderProcedureType instance.
     *
     * @param value
     *      tender procedure type
     * @return OCDS procedure method
     */
    public static OCDSProcedureMethod from(final TenderProcedureType value) {
        if (value == null) {
            return null;
        }

        switch (value) {
            case OPEN: case COMPETITIVE_DIALOG: case DESIGN_CONTEST: case PUBLIC_CONTEST: case INOVATION_PARTNERSHIP:
                return OPEN;
            case RESTRICTED: case NEGOTIATED_WITH_PUBLICATION: case MINITENDER: case DPS_PURCHASE: case NEGOTIATED:
                return SELECTIVE;
            case NEGOTIATED_WITHOUT_PUBLICATION: case OUTRIGHT_AWARD:
                return DIRECT;
            case APPROACHING_BIDDERS:
                return LIMITED;
            default:
                return null;
        }
    }
}
