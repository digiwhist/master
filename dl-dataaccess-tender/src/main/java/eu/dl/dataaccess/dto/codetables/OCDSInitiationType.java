package eu.dl.dataaccess.dto.codetables;

import com.fasterxml.jackson.annotation.JsonValue;

/**
 * OCDS initiation type enumeration.
 *
 * @author Tomas Mrazek
 */
public enum OCDSInitiationType {
    /**
     * Tender.
     */
    TENDER;

    @Override
    @JsonValue
    public String toString() {
        return OCDSEnumUtils.ocdsCodelistJsonValue(this);
    }
}
