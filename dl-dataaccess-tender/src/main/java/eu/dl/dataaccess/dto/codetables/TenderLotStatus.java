package eu.dl.dataaccess.dto.codetables;

/**
 * Tender lot status.
 */
public enum TenderLotStatus {
    /**
     * Prepared.
     */
    PREPARED,

    /**
     * Publicly announced, bids are accepted or negotiated.
     */
    ANNOUNCED,

    /**
     * Awarded and being fulfilled.
     */
    AWARDED,

    /**
     * Cancelled.
     */
    CANCELLED,

    /**
     * Fulfilled and paid.
     */
    FINISHED;
    
    /**
     * Contains mapping between lot status and publication form type.
     * @param formType publication form type
     * @return value from this enum
     */
    public static TenderLotStatus fromPublicationFormType(final PublicationFormType formType) {
        if (formType == PublicationFormType.PRIOR_INFORMATION_NOTICE) {
            return TenderLotStatus.PREPARED;
        }
        if (formType == PublicationFormType.CONTRACT_NOTICE) {
            return TenderLotStatus.ANNOUNCED;
        }
        if (formType == PublicationFormType.CONTRACT_AWARD) {
            return TenderLotStatus.AWARDED;
        }
        if (formType == PublicationFormType.CONTRACT_CANCELLATION) {
            return TenderLotStatus.CANCELLED;
        }
        return null;
    }
}
