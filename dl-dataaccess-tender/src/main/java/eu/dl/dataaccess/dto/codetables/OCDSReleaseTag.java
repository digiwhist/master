package eu.dl.dataaccess.dto.codetables;

import com.fasterxml.jackson.annotation.JsonValue;

/**
 * OCDS Release tag enumeration.
 *
 * @author Tomas Mrazek
 */
public enum OCDSReleaseTag {
    /**
     * A contracting process is proposed or planned. Information in the tender section describes the proposed process.
     * The tender.status field should be used to identify whether the planning is at an early pipeline stage,
     * or whether there are detailed plans for a tender developed.
     */
    PLANNING,

    /**
     * Details of a proposed or planned contracting process are being updated. This may include addition of information
     * and documents from consultation engagement activities, revised details or timelines for a proposed contracting
     * process, or a tender.status update to indicate that a pipeline proposal has been withdrawn.
     */
    PLANNING_UPDATE,

    /**
     * Providing information about a new tender (call for proposals) process. Tender release should contain details of
     * the goods or services being sought.
     */
    TENDER,

    /**
     * An amendment to an existing tender release. There should be at least one tender release with the same ocid,
     * but an earlier releaseDate, before a tenderAmendment is published. The term amendment has legal meaning in many
     * jurisdictions.
     */
    TENDER_AMENDMENT,

    /**
     * An update to an existing tender release. There should be at least one tender release with the same ocid, but an
     * earlier releaseDate, before a tenderUpdate is published. An update may add new information or make corrections
     * to prior published information. It should not be used for formal legal amendments to a tender, for which the
     * tenderAmendment tag should be used.
     */
    TENDER_UPDATE,

    /**
     * The cancellation of an existing tender. There should be at least one release with the same ocid, but an earlier
     * releaseDate, which provides details of the tender being cancelled.
     */
    TENDER_CANCELLATION,

    /**
     * Providing information about the award of a contract. The tender section may be populated with details of the
     * process leading up to the award, and one or more award sections will be present.
     */
    AWARD,

    /**
     * An update to an existing award release. There should be at least one award release with the same ocid, but an
     * earlier releaseDate before an awardUpdate is published. An update may add new information or make corrections.
     */
    AWARD_UPDATE,

    /**
     * Providing information about the cancellation of an award.
     */
    AWARD_CANCELLATION,

    /**
     * Providing information about the details of a contract that has been, or will be, entered into. The tender section
     * may be populated with details of the process leading up to the contract, and the award section may contain
     * details of the award on the basis of which this contract will be signed.
     */
    CONTRACT,

    /**
     * Providing information about updates to a contract. There should be at least one contract release with the same
     * ocid, but an earlier releaseDate, before a contractUpdate is published. An update may add new information or
     * make corrections to prior published information. It should not be used for formal legal amendments to a contract,
     * for which the contractAmendment tag should be used.
     */
    CONTRACT_UPDATE,

    /**
     * An amendment to an existing contract release. There should be at least one contract release with the same ocid,
     * but an earlier releaseDate, before a contractAmendment is published. The term amendment has legal meaning in many
     * jurisdictions.
     */
    CONTRACT_AMENDMENT,

    /**
     * Providing new information on the implementation of a contracting process.
     */
    IMPLEMENTATION,

    /**
     * Updating existing information provided about the implementation of a contracting process.
     */
    IMPLEMENTATION_UPDATE,

    /**
     * Providing information at the end of a contracting process.
     */
    CONTRACT_TERMINATION,

    /**
     * This tag is used only in compiled records, which have merged together multiple releases to provide a snapshot
     * view of the contract, and a version history.
     */
    COMPILED;
    
    @Override
    @JsonValue
    public String toString() {
        return OCDSEnumUtils.ocdsCodelistJsonValue(this);
    }
}
