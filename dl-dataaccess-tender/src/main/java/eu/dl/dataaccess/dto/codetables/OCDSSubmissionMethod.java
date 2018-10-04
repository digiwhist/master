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
    ELECTRONIC_AUCTION,

    /**
     * In person.
     */
    IN_PERSON,

    /**
     * Electronic Submission.
     */
    ELECTRONIC_SUBMISSION,
    /**
     * Written.
     */
    WRITTEN;

    @Override
    @JsonValue
    public String toString() {
        return OCDSEnumUtils.ocdsCodelistJsonValue(this);
    }
}
