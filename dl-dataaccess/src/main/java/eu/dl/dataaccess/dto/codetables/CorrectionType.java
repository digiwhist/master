package eu.dl.dataaccess.dto.codetables;

/**
 * Correction type.
 */
public enum CorrectionType {
    /**
     * Bid deadline correction.
     */
    BID_DEADLINE,

    /**
     * CPVs correction.
     */
    CPVS,

    /**
     * Tender final price correction.
     */
    FINAL_PRICE,

    /**
     * Bid price correction.
     */
    BID_PRICE,

    /**
     * Tender estimated price correction.
     */
    ESTIMATED_PRICE,

    /**
     * Lot estimated price correction.
     */
    LOT_ESTIMATED_PRICE,

    /**
     * Price correction.
     */
    PRICE;
}
