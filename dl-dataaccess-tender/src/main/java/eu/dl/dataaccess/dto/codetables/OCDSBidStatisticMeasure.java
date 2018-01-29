package eu.dl.dataaccess.dto.codetables;

import com.fasterxml.jackson.annotation.JsonValue;

/**
 * OCDS statistic measure type enumeration.
 *
 * @author Tomas Mrazek
 */
public enum OCDSBidStatisticMeasure {
    /**
     * Bids count.
     */
    BIDS,

    /**
     * Electronic bids count.
     */

    ELECTRONIC_BIDS,
    /**
     * Valid bids count.
     */
    VALID_BIDS,

    /**
     * SME bids count.
     */
    SME_BIDS,

    /**
     * Foreign dids count.
     */
    FOREIGN_BIDS,

    /**
     * Foreign bids from EU count.
     */
    FOREIGN_BIDS_FROM_EU;

    @Override
    @JsonValue
    public String toString() {
        if (this.equals(FOREIGN_BIDS_FROM_EU)) {
            return "foreignBidsFromEU";
        }

        return OCDSEnumUtils.ocdsCodelistJsonValue(this);
    }
}
