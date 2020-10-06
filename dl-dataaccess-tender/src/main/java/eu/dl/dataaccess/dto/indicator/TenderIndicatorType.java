package eu.dl.dataaccess.dto.indicator;

/**
 * Enum with possible indicator types.
 */
public enum TenderIndicatorType {
    /**
     * Corruption - single bid.
     */
    INTEGRITY_SINGLE_BID,

    /**
     * Corruption - new company.
     */
    INTEGRITY_NEW_COMPANY,

    /**
     * Corruption - decision period length.
     */
    INTEGRITY_DECISION_PERIOD,

    /**
     * Corruption - advertisement period length.
     */
    INTEGRITY_ADVERTISEMENT_PERIOD,

    /**
     * Corruption - call for tenders publication.
     */
    INTEGRITY_CALL_FOR_TENDER_PUBLICATION,
    
    /**
     * Corruption - procedure type.
     */
    INTEGRITY_PROCEDURE_TYPE,
    
    /**
     * Corruption - tax haven.
     */
    INTEGRITY_TAX_HAVEN,

    /**
     * Corruption - political connections of suppliers.
     */
    INTEGRITY_POLITICAL_CONNECTIONS,

    /**
     * Corruption - cost overrun.
     */
    INTEGRITY_COST_OVERRUN,

    /**
     * Administrative capacity - centralized procurement.
     */
    ADMINISTRATIVE_CENTRALIZED_PROCUREMENT,

    /**
     * Administrative capacity - electronic auction.
     */
    ADMINISTRATIVE_ELECTRONIC_AUCTION,

    /**
     * Administrative capacity - framework agreement.
     */
    ADMINISTRATIVE_FRAMEWORK_AGREEMENT,

    /**
     * Administrative capacity - covered by GPA.
     */
    ADMINISTRATIVE_COVERED_BY_GPA,

    /**
     * Administrative capacity - english as foreign language.
     */
    ADMINISTRATIVE_ENGLISH_AS_FOREIGN_LANGUAGE,

    /**
     * Transparency - number of key missing fields.
     */
    TRANSPARENCY_NUMBER_OF_KEY_MISSING_FIELDS,

    /**
     * Administrative - notice and award discrepancies.
     */
    ADMINISTRATIVE_NOTICE_AND_AWARD_DISCREPANCIES,

    /**
     * Transparency - missing award decision date.
     */
    TRANSPARENCY_AWARD_DATE_MISSING,

    /**
     * Transparency - missing buyer name.
     */
    TRANSPARENCY_BUYER_NAME_MISSING,

    /**
     * Transparency - missing procedure type.
     */
    TRANSPARENCY_PROC_METHOD_MISSING,

    /**
     * Transparency - missing buyer nuts.
     */
    TRANSPARENCY_BUYER_LOC_MISSING,

    /**
     * Transparency - missing bidder id.
     */
    TRANSPARENCY_BIDDER_ID_MISSING,

    /**
     * Transparency - missing bidder name.
     */
    TRANSPARENCY_BIDDER_NAME_MISSING,

    /**
     * Transparency - missing tender CPVs.
     */
    TRANSPARENCY_MARKET_MISSING,

    /**
     * Transparency - missing tender title.
     */
    TRANSPARENCY_TITLE_MISSING,

    /**
     * Transparency - missing bid price.
     */
    TRANSPARENCY_VALUE_MISSING,

    /**
     * Transparency - missing year.
     */
    TRANSPARENCY_YEAR_MISSING,

    /**
     * Integrity - winner's contract share by buyers.
     */
    INTEGRITY_WINNER_CA_SHARE
}

