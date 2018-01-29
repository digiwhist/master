package eu.dl.dataaccess.dto.codetables;

import com.fasterxml.jackson.annotation.JsonValue;

/**
 * OCDS submision method enumeration.
 *
 * @author Tomas Mrazek
 */
public enum OCDSSubmissionMethod {
    /**
     * Electronic auction.
     */
    ELECTRONIC_AUCTION;

    @Override
    @JsonValue
    public String toString() {
        return OCDSEnumUtils.ocdsCodelistJsonValue(this);
    }
}
