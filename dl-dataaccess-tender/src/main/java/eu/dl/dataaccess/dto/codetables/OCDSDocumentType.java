package eu.dl.dataaccess.dto.codetables;

import com.fasterxml.jackson.annotation.JsonValue;

/**
 * OCDS document type enumeration.
 *
 * @author Tomas Mrazek
 */
public enum OCDSDocumentType {
    /**
     * Procurement plan.
     */
    PROCUREMENT_PLAN,
    /**
     * Market studies.
     */
    MARKET_STUDIES,
    /**
     * Bidding documents.
     */
    BIDDING_DOCUMENTS,
    /**
     * Eligibility criteria.
     */
    ELIGIBILITY_CRITERIA,
    /**
     * Clarification.
     */
    CLARIFICATIONS,
    /**
     * Shortlisted firms.
     */
    SHORTLISTED_FIRMS,
    /**
     * Evaluation reports.
     */
    EVALUATION_REPORTS,
    /**
     * Complaints.
     */
    COMPLAINTS,
    /**
     * Award notice.
     */
    AWARD_NOTICE,
    /**
     * Contract draft.
     */
    CONTRACT_DRAFT,
    /**
     * Contract annexe.
     */
    CONTRACT_ANNEXE,
    /**
     * Final audit.
     */
    FINAL_AUDIT,
    /**
     * Tender notice.
     */
    TENDER_NOTICE,
    /**
     * Cancellation details.
     */
    CANCELLATION_DETAILS,
    /**
     * Other.
     */
    OTHER,
    /**
     * Prior information notice.
     */
    PRIOR_INFORMATION_NOTICE,
    /**
     * Implementation.
     */
    IMPLEMENTATION,
    /**
     * Contract update.
     */
    TENDER_UPDATE,
    /**
     * Contract amendment.
     */
    AMENDMENT;

    @Override
    @JsonValue
    public String toString() {
        return OCDSEnumUtils.ocdsCodelistJsonValue(this);
    }

    /**
     * Returns OCDS document type for the given document type.
     *
     * @param value
     *      document type
     * @return OCDS document type
     */
    public static OCDSDocumentType from(final DocumentType value) {
        if (value == null) {
            return null;
        }

        switch (value) {
            case PROJECT_PLAN:
                return PROCUREMENT_PLAN;
            case MARKET_RESEARCH:
                return MARKET_STUDIES;
            case SPECIFICATIONS: case CALL_FOR_TENDERS: case TENDER:
                return BIDDING_DOCUMENTS;
            case QUALIFICATION_DOCUMENTS:
                return ELIGIBILITY_CRITERIA;
            case ADDITIONAL_INFORMATION:
                return CLARIFICATIONS;
            case CANDIDATES_NUMBER_LIMITATION_PROTOCOL:
                return SHORTLISTED_FIRMS;
            case QUALIFICATIONS_EVALUATION_PROTOCOL: case BIDDER_NEGOTIATION_PROTOCOL:
            case EVALUATION_COMMITTEE_PROTOCOL: case TENDERS_EVALUATION_PROTOCOL:
                return EVALUATION_REPORTS;
            case OBJECTION:
                return COMPLAINTS;
            case BEST_TENDER_SELECTION_NOTICE:
                return AWARD_NOTICE;
            case CONTRACTOR_AGREEMENT:
                return CONTRACT_DRAFT;
            case AGREEMENT_AMENDMENT: case AGREEMENT_APPENDIX:
                return CONTRACT_ANNEXE;
            case BUYER_REPORT:
                return FINAL_AUDIT;
            default:
                return null;
        }
    }

    /**
     * Returns OCDS document type for the given publication form type.
     *
     * @param value
     *      publication form type
     * @return OCDS document type
     */
    public static OCDSDocumentType from(final PublicationFormType value) {
        if (value == null) {
            return null;
        }

        switch (value) {
            case CONTRACT_AWARD:
                return AWARD_NOTICE;
            case CONTRACT_NOTICE:
                return TENDER_NOTICE;
            case CONTRACT_CANCELLATION:
                return CANCELLATION_DETAILS;
            case OTHER:
                return OTHER;
            case PRIOR_INFORMATION_NOTICE:
                return PRIOR_INFORMATION_NOTICE;
            case CONTRACT_IMPLEMENTATION:
                return IMPLEMENTATION;
            case CONTRACT_UPDATE:
                return TENDER_UPDATE;
            case CONTRACT_AMENDMENT:
                return AMENDMENT;
            default:
                return null;
        }
    }
}
