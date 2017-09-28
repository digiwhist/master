package eu.dl.dataaccess.dto.indicator;

/**
 * Enum with possible indicator types.
 */
public enum TenderIndicatorType {
    /**
     * Corruption - single bid.
     */
    CORRUPTION_SINGLE_BID,

    /**
     * Corruption - new company.
     */
    CORRUPTION_NEW_COMPANY,

    /**
     * Corruption - decision period length.
     */
    CORRUPTION_DECISION_PERIOD,

    /**
     * Corruption - advertisement period length.
     */
    CORRUPTION_ADVERTISEMENT_PERIOD,

    /**
     * Corruption - prior information notice.
     */
    CORRUPTION_PRIOR_INFORMATION_NOTICE,
    
    /**
     * Corruption - procedure type.
     */
    CORRUPTION_PROCEDURE_TYPE,
    
    /**
     * Corruption - tax haven.
     */
    CORRUPTION_TAX_HAVEN,

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
}

