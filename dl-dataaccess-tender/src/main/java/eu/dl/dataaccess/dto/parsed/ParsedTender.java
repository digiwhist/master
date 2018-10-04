package eu.dl.dataaccess.dto.parsed;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

/**
 * Public Contract. Contains full info about single contract.
 */
public class ParsedTender extends BaseParsedTenderLot<ParsedTender> implements Parsable {
    /**
     * Reference number given to contract by buyer.
     */
    private String buyerAssignedId;

    /**
     * Type of procedure.
     */
    private String procedureType;

    /**
     * Type of procedure - unstructured.
     */
    private String nationalProcedureType;

    /**
     * Accelerated procedure.
     */
    private String isAcceleratedProcedure;

    /**
     * Reason for accelerated procedure.
     */
    private String acceleratedProcedureJustification;

    /**
     * Maximum number of bids, typically used in Restricted or Negotiated with
     * publication procedure.
     */
    private String maxBidsCount;

    /**
     * Type of supplies (services/goods/construction).
     */
    private String supplyType;

    /**
     * Tender size (below/above the threshold).
     */
    private String size;

    /**
     * Date until which bids need to be submitted (do not confuse with
     * application deadline for Restricted procedure type).
     */
    private String bidDeadline;

    /**
     * Date until tender documents or additional information are provided.
     */
    private String documentsDeadline;

    /**
     * Price is asked for tender documents provision.
     */
    private String documentsPayable;

    /**
     * Price of tender documents.
     */
    private ParsedPrice documentsPrice;

    /**
     * Location where tender documentation can be obtained (url or name of
     * organisation).
     */
    private ParsedAddress documentsLocation;

    /**
     * Free/restricted access to documents.
     */
    private String isDocumentsAccessRestricted;

    /**
     * Tender is awarded by central purchasing authority.
     */
    private String isCentralProcurement;

    /**
     * Tender involves joint procurement.
     */
    private String isJointProcurement;

    /**
     * Identification of buyer(contracting authority) or other purchasing body.
     */
    private List<ParsedBody> buyers;

    /**
     * The purchase is being made for someone else. e.g. city purchases on
     * behalf of one of its schools.
     */
    private String isOnBehalfOf;

    /**
     * Bodies on whose behalf the purchase is being made on.
     */
    private List<ParsedBody> onBehalfOf;

    /**
     * Body from whom further information can be obtained.
     */
    private ParsedBody furtherInformationProvider;

    /**
     * Body from whom specifications and additional documents can be obtained.
     */
    private ParsedBody specificationsProvider;

    /**
     * Body to whom tenders/requests to participate must be sent.
     */
    private ParsedBody bidsRecipient;

    /**
     * List of relevant publications.
     */
    private List<ParsedPublication> publications;

    /**
     * Data on external contract administrators, to whom the procedure
     * administration was outsourced (in case it was).
     */
    private List<ParsedBody> administrators;

    /**
     * Data on external supervisors, to whom the supervision over the contract
     * fulfillment was outsourced (in case it was).
     */
    private List<ParsedBody> supervisors;

    /**
     * Data on external body, to whom the preparation of tender documents was
     * outsourced (in case it was).
     */
    private ParsedBody specificationsCreator;

    /**
     * Tender is divided into lots.
     */
    private String hasLots;

    /**
     * List of contract lots.
     */
    private List<ParsedTenderLot> lots;

    /**
     * List of candidates who expressed interest in contract (typically in
     * Restricted procedure or Negotiated procedure with publication).
     */
    private List<ParsedBody> candidates;

    /**
     * List of potential suppliers actively approached or consulted by
     * contracting authority with information on contract.
     */
    private List<ParsedBody> approachedBidders;

    /**
     * List of documents relevant to contract (tender documentation, protocol on
     * bids evaluation etc).
     */
    private List<ParsedDocument> documents;

    /**
     * Links to information in case, there has been any form of court proceeding
     * related to contract (commenced rather than consluded necessarily).
     */
    private List<String> courtProceedings;

    /**
     * Links to information in case, there has been a court intervention into
     * the tender/contract.
     */
    private List<String> courtInterventions;

    /**
     * Reasons for use of negotiated procedure without publication.
     */
    private List<String> npwpReasons;

    /**
     * Description of deposits required.
     */
    private String deposits;

    /**
     * Description of personal requirements on bidder.
     */
    private String personalRequirements;

    /**
     * Description of economic requirements on bidder.
     */
    private String economicRequirements;

    /**
     * Description of technical requirements on bidder.
     */
    private String technicalRequirements;

    /**
     * Name of body to which appeals should be filed.
     */
    private String appealBodyName;

    /**
     * Name of body to which appeals should be filed.
     */
    private String mediationBodyName;

    /**
     * Justification for framework agreement going over 4 years.
     */
    private String excessiveFrameworkAgreementJustification;

    /**
     * Cancellation of the whole tender or some of its lots only.
     */
    private String isWholeTenderCancelled;

    /**
     * Deadline date for sending enquiries to the subject.
     */
    private String enquiryDeadline;

    /**
     * Deadline for awarding the contract (how long are bidders bound by their
     * bids).
     */
    private String awardDeadline;

    /**
     * How long are bidders bound by their bids (from the date stated for receipt of tender).
     */
    private String awardDeadlineDuration;

    /**
     * Final value of contract.
     */
    private ParsedPrice finalPrice;

    /**
     * Languages in which tenders or requests to participate may be submitted.
     */
    private List<String> eligibleBidLanguages;

    /**
     * Electronic invoicing will be accepted.
     */
    private String isEInvoiceAccepted;

    /**
     * List of corrections.
     */
    private List<ParsedCorrigendum> corrections;

    /**
     * Reason for modification.
     */
    private String modificationReason;

    /**
     * Detailed description of modification reason.
     */
    private String modificationReasonDescription;

    /**
     * Additional info.
     */
    private String additionalInfo;

    /**
     * Country of origin.
     */
    private String country;


    /**
     * Created date of raw object.
     */
    private LocalDateTime createdRaw;

    /**
     * @return the buyerAssignedId
     */
    public final String getBuyerAssignedId() {
        return buyerAssignedId;
    }

    /**
     * @param buyerAssignedId
     *         the buyerAssignedId to set
     *
     * @return this instance for chaining
     */

    public final ParsedTender setBuyerAssignedId(final String buyerAssignedId) {
        this.buyerAssignedId = buyerAssignedId;
        return this;
    }

    /**
     * @return the procedureType
     */

    
    public final String getProcedureType() {
        return procedureType;
    }

    /**
     * @param procedureType
     *         the procedureType to set
     *
     * @return this instance for chaining
     */

    public final ParsedTender setProcedureType(final String procedureType) {
        this.procedureType = procedureType;
        return this;
    }

    /**
     * @return the nationalProcedureType
     */

    
    public final String getNationalProcedureType() {
        return nationalProcedureType;
    }

    /**
     * @param nationalProcedureType
     *         the nationalProcedureType to set
     *
     * @return this instance for chaining
     */

    public final ParsedTender setNationalProcedureType(final String nationalProcedureType) {
        this.nationalProcedureType = nationalProcedureType;
        return this;
    }

    /**
     * @return is the procedure accelerated
     */

    
    public final String getIsAcceleratedProcedure() {
        return isAcceleratedProcedure;
    }

    /**
     * @param isAcceleratedProcedure
     *         is the procedure accelerated
     *
     * @return this instance for chaining
     */

    public final ParsedTender setIsAcceleratedProcedure(final String isAcceleratedProcedure) {
        this.isAcceleratedProcedure = isAcceleratedProcedure;
        return this;
    }

    /**
     * @return reason for the accelerated procedure
     */

    
    public final String getAcceleratedProcedureJustification() {
        return acceleratedProcedureJustification;
    }

    /**
     * @param acceleratedProcedureJustification
     *         reason for the accelerated procedure
     *
     * @return this instance for chaining
     */

    public final ParsedTender setAcceleratedProcedureJustification(
            final String acceleratedProcedureJustification) {
        this.acceleratedProcedureJustification = acceleratedProcedureJustification;
        return this;
    }

    /**
     * @return the maxBidsCount
     */

    
    public final String getMaxBidsCount() {
        return maxBidsCount;
    }

    /**
     * @param maxBidsCount
     *         the maxBidsCount to set
     *
     * @return this instance for chaining
     */

    public final ParsedTender setMaxBidsCount(final String maxBidsCount) {
        this.maxBidsCount = maxBidsCount;
        return this;
    }

    /**
     * @return the supplyType
     */

    
    public final String getSupplyType() {
        return supplyType;
    }

    /**
     * @param supplyType
     *         the supplyType to set
     *
     * @return this instance for chaining
     */

    public final ParsedTender setSupplyType(final String supplyType) {
        this.supplyType = supplyType;
        return this;
    }

    /**
     * @return the size
     */

    
    public final String getSize() {
        return size;
    }

    /**
     * @param size
     *         the size to set
     *
     * @return this instance for chaining
     */

    public final ParsedTender setSize(final String size) {
        this.size = size;
        return this;
    }

    /**
     * @return the bidDeadline
     */

    
    public final String getBidDeadline() {
        return bidDeadline;
    }

    /**
     * @param bidDeadline
     *         the bidDeadline to set
     *
     * @return this instance for chaining
     */

    public final ParsedTender setBidDeadline(final String bidDeadline) {
        this.bidDeadline = bidDeadline;
        return this;
    }

    /**
     * @return the documentsDeadline
     */

    
    public final String getDocumentsDeadline() {
        return documentsDeadline;
    }

    /**
     * @param documentsDeadline
     *         the documentsDeadline to set
     *
     * @return this instance for chaining
     */

    public final ParsedTender setDocumentsDeadline(final String documentsDeadline) {
        this.documentsDeadline = documentsDeadline;
        return this;
    }

    /**
     * @return the documentsPayable
     */

    
    public final String getDocumentsPayable() {
        return documentsPayable;
    }

    /**
     * @param documentsPayable
     *         the documentsPayable to set
     *
     * @return this instance for chaining
     */

    public final ParsedTender setDocumentsPayable(final String documentsPayable) {
        this.documentsPayable = documentsPayable;
        return this;
    }

    /**
     * @return the documentsPrice
     */

    
    public final ParsedPrice getDocumentsPrice() {
        return documentsPrice;
    }

    /**
     * @param documentsPrice
     *         the documentsPrice to set
     *
     * @return this instance for chaining
     */

    public final ParsedTender setDocumentsPrice(final ParsedPrice documentsPrice) {
        this.documentsPrice = documentsPrice;
        return this;
    }

    /**
     * @return the documentsLocation
     */

    
    public final ParsedAddress getDocumentsLocation() {
        return documentsLocation;
    }

    /**
     * @param documentsLocation
     *         the documentsLocation to set
     *
     * @return this instance for chaining
     */

    public final ParsedTender setDocumentsLocation(final ParsedAddress documentsLocation) {
        this.documentsLocation = documentsLocation;
        return this;
    }

    /**
     * @return true if the access to documents is free, false for restricted access
     */

    
    public final String getIsDocumentsAccessRestricted() {
        return isDocumentsAccessRestricted;
    }

    /**
     * @param isDocumentsAccessRestricted
     *         boolean whether the access to documents is free
     *
     * @return this instance for chaining
     */

    public final ParsedTender setIsDocumentsAccessRestricted(final String isDocumentsAccessRestricted) {
        this.isDocumentsAccessRestricted = isDocumentsAccessRestricted;
        return this;
    }

    /**
     * @return the isCentralProcurement
     */

    
    public final String getIsCentralProcurement() {
        return isCentralProcurement;
    }

    /**
     * @param isCentralProcurement
     *         the isCentralProcurement to set
     *
     * @return this instance for chaining
     */

    public final ParsedTender setIsCentralProcurement(final String isCentralProcurement) {
        this.isCentralProcurement = isCentralProcurement;
        return this;
    }

    /**
     * @return the isJointProcurement
     */

    
    public final String getIsJointProcurement() {
        return isJointProcurement;
    }

    /**
     * @param isJointProcurement
     *         the isJointProcurement to set
     *
     * @return this instance for chaining
     */

    public final ParsedTender setIsJointProcurement(final String isJointProcurement) {
        this.isJointProcurement = isJointProcurement;
        return this;
    }

    /**
     * @return the buyers
     */

    
    public final List<ParsedBody> getBuyers() {
        return buyers;
    }

    /**
     * @param buyers
     *         the buyers to set
     *
     * @return this instance for chaining
     */

    public final ParsedTender setBuyers(final List<ParsedBody> buyers) {
        this.buyers = buyers;
        return this;
    }

    /**
     * @return boolean string whether the purchase is being made for someone else
     */

    
    public final String getIsOnBehalfOf() {
        return isOnBehalfOf;
    }

    /**
     * @param isOnBehalfOf
     *         boolean string whether the purchase is being made for someone else
     *
     * @return this instance for chaining
     */

    public final ParsedTender setIsOnBehalfOf(final String isOnBehalfOf) {
        this.isOnBehalfOf = isOnBehalfOf;
        return this;
    }

    /**
     * @return the list of onBehalfOf
     */

    
    public final List<ParsedBody> getOnBehalfOf() {
        return onBehalfOf;
    }

    /**
     * @param onBehalfOf
     *         the list of onBehalfOf to set
     *
     * @return this instance for chaining
     */

    public final ParsedTender setOnBehalfOf(final List<ParsedBody> onBehalfOf) {
        this.onBehalfOf = onBehalfOf;
        return this;
    }

    /**
     * @param newOnBehalfOf
     *         the onBehalfOf to add
     *
     * @return this instance for chaining
     */
    public final ParsedTender addOnBehalfOf(final ParsedBody newOnBehalfOf) {
        if (newOnBehalfOf != null) {
            if (getOnBehalfOf() == null) {
                setOnBehalfOf(new ArrayList<>());
            }

            this.onBehalfOf.add(newOnBehalfOf);
        }

        return this;
    }

    /**
     * @return body from whom further information can be obtained
     */

    
    public final ParsedBody getFurtherInformationProvider() {
        return furtherInformationProvider;
    }

    /**
     * @param furtherInformationProvider
     *         body from whom further information can be obtained
     *
     * @return this instance for chaining
     */

    public final ParsedTender setFurtherInformationProvider(final ParsedBody furtherInformationProvider) {
        this.furtherInformationProvider = furtherInformationProvider;
        return this;
    }

    /**
     * @return body from whom specifications and additional documents can be obtained
     */

    
    public final ParsedBody getSpecificationsProvider() {
        return specificationsProvider;
    }

    /**
     * @param specificationsProvider
     *         body from whom specifications and additional documents can be obtained
     *
     * @return this instance for chaining
     */

    public final ParsedTender setSpecificationsProvider(final ParsedBody specificationsProvider) {
        this.specificationsProvider = specificationsProvider;
        return this;
    }

    /**
     * @return body to whom tenders/requests to participate must be sent
     */

    
    public final ParsedBody getBidsRecipient() {
        return bidsRecipient;
    }

    /**
     * @param bidsRecipient
     *         body to whom tenders/requests to participate must be sent
     *
     * @return this instance for chaining
     */

    public final ParsedTender setBidsRecipient(final ParsedBody bidsRecipient) {
        this.bidsRecipient = bidsRecipient;
        return this;
    }

    /**
     * @return the publications
     */

    
    public final List<ParsedPublication> getPublications() {
        return publications;
    }

    /**
     * @param publications
     *         the publications to set
     *
     * @return this instance for chaining
     */

    public final ParsedTender setPublications(final List<ParsedPublication> publications) {
        this.publications = publications;
        return this;
    }

    /**
     * @return the administrators
     */

    
    public final List<ParsedBody> getAdministrators() {
        return administrators;
    }

    /**
     * @param administrators
     *         the administrators to set
     *
     * @return this instance for chaining
     */

    public final ParsedTender setAdministrators(final List<ParsedBody> administrators) {
        this.administrators = administrators;
        return this;
    }

    /**
     * @return the supervisors
     */

    
    public final List<ParsedBody> getSupervisors() {
        return supervisors;
    }

    /**
     * @param supervisors
     *         the supervisors to set
     *
     * @return this instance for chaining
     */

    public final ParsedTender setSupervisors(final List<ParsedBody> supervisors) {
        this.supervisors = supervisors;
        return this;
    }

    /**
     * @return the specificationsCreator
     */

    
    public final ParsedBody getSpecificationsCreator() {
        return specificationsCreator;
    }

    /**
     * @param specificationsCreator
     *         the specificationsCreator to set
     *
     * @return this instance for chaining
     */

    public final ParsedTender setSpecificationsCreator(final ParsedBody specificationsCreator) {
        this.specificationsCreator = specificationsCreator;
        return this;
    }

    /**
     * @return is tender divided into lots
     */

    
    public final String getHasLots() {
        return hasLots;
    }

    /**
     * @param hasLots
     *         is tender divided into lots
     *
     * @return this instance for chaining
     */

    public final ParsedTender setHasLots(final String hasLots) {
        this.hasLots = hasLots;
        return this;
    }

    /**
     * @return the lots
     */

    
    public final List<ParsedTenderLot> getLots() {
        return lots;
    }

    /**
     * @param lots
     *         the lots to set
     *
     * @return this instance for chaining
     */

    public final ParsedTender setLots(final List<ParsedTenderLot> lots) {
        this.lots = lots;
        return this;
    }

    /**
     * @return the candidates
     */

    
    public final List<ParsedBody> getCandidates() {
        return candidates;
    }

    /**
     * @param candidates
     *         the candidates to set
     *
     * @return this instance for chaining
     */

    public final ParsedTender setCandidates(final List<ParsedBody> candidates) {
        this.candidates = candidates;
        return this;
    }

    /**
     * @return the approachedBidders
     */

    
    public final List<ParsedBody> getApproachedBidders() {
        return approachedBidders;
    }

    /**
     * @param approachedBidders
     *         the approachedBidders to set
     *
     * @return this instance for chaining
     */

    public final ParsedTender setApproachedBidders(final List<ParsedBody> approachedBidders) {
        this.approachedBidders = approachedBidders;
        return this;
    }

    /**
     * @return the documents
     */

    
    public final List<ParsedDocument> getDocuments() {
        return documents;
    }

    /**
     * @param documents
     *         the documents to set
     *
     * @return this instance for chaining
     */

    public final ParsedTender setDocuments(final List<ParsedDocument> documents) {
        this.documents = documents;
        return this;
    }

    /**
     * @return the courtProceedings
     */

    
    public final List<String> getCourtProceedings() {
        return courtProceedings;
    }

    /**
     * @param courtProceedings
     *         the courtProceedings to set
     *
     * @return this instance for chaining
     */

    public final ParsedTender setCourtProceedings(final List<String> courtProceedings) {
        this.courtProceedings = courtProceedings;
        return this;
    }

    /**
     * @return the courtInterventions
     */

    
    public final List<String> getCourtInterventions() {
        return courtInterventions;
    }

    /**
     * @param courtInterventions
     *         the courtInterventions to set
     *
     * @return this instance for chaining
     */

    public final ParsedTender setCourtInterventions(final List<String> courtInterventions) {
        this.courtInterventions = courtInterventions;
        return this;
    }

    /**
     * @return the npwpReasons
     */

    
    public final List<String> getNpwpReasons() {
        return npwpReasons;
    }

    /**
     * @param npwpReasons
     *         the npwpReasons to set
     *
     * @return this instance for chaining
     */

    public final ParsedTender setNpwpReasons(final List<String> npwpReasons) {
        this.npwpReasons = npwpReasons;
        return this;
    }

    /**
     * @return the deposits
     */

    
    public final String getDeposits() {
        return deposits;
    }

    /**
     * @param deposits
     *         the deposits to set
     *
     * @return this instance for chaining
     */

    public final ParsedTender setDeposits(final String deposits) {
        this.deposits = deposits;
        return this;
    }

    /**
     * @return the personalRequirements
     */

    
    public final String getPersonalRequirements() {
        return personalRequirements;
    }

    /**
     * @param personalRequirements
     *         the personalRequirements to set
     *
     * @return this instance for chaining
     */

    public final ParsedTender setPersonalRequirements(final String personalRequirements) {
        this.personalRequirements = personalRequirements;
        return this;
    }

    /**
     * @return the economicRequirements
     */

    
    public final String getEconomicRequirements() {
        return economicRequirements;
    }

    /**
     * @param economicRequirements
     *         the economicRequirements to set
     *
     * @return this instance for chaining
     */

    public final ParsedTender setEconomicRequirements(final String economicRequirements) {
        this.economicRequirements = economicRequirements;
        return this;
    }

    /**
     * @return the technicalRequirements
     */

    
    public final String getTechnicalRequirements() {
        return technicalRequirements;
    }

    /**
     * @param technicalRequirements
     *         the technicalRequirements to set
     *
     * @return this instance for chaining
     */

    public final ParsedTender setTechnicalRequirements(final String technicalRequirements) {
        this.technicalRequirements = technicalRequirements;
        return this;
    }

    /**
     * @return the appealBodyName
     */

    
    public final String getAppealBodyName() {
        return appealBodyName;
    }

    /**
     * @param appealBodyName
     *         the appealBodyName to set
     *
     * @return this instance for chaining
     */

    public final ParsedTender setAppealBodyName(final String appealBodyName) {
        this.appealBodyName = appealBodyName;
        return this;
    }

    /**
     * @return the mediationBodyName
     */

    
    public final String getMediationBodyName() {
        return mediationBodyName;
    }

    /**
     * @param mediationBodyName
     *         the mediationBodyName to set
     *
     * @return this instance for chaining
     */

    public final ParsedTender setMediationBodyName(final String mediationBodyName) {
        this.mediationBodyName = mediationBodyName;
        return this;
    }

    /**
     * @return justification for framework agreement going over 4 years
     */

    
    public final String getExcessiveFrameworkAgreementJustification() {
        return excessiveFrameworkAgreementJustification;
    }

    /**
     * @param excessiveFrameworkAgreementJustification
     *         justification for framework agreement going over 4 years
     *
     * @return this instance for chaining
     */

    public final ParsedTender setExcessiveFrameworkAgreementJustification(
            final String excessiveFrameworkAgreementJustification) {
        this.excessiveFrameworkAgreementJustification = excessiveFrameworkAgreementJustification;
        return this;
    }

    /**
     * @return true if the whole tender is cancelled, false if just some of its lots
     */

    
    public final String getIsWholeTenderCancelled() {
        return isWholeTenderCancelled;
    }

    /**
     * @param isWholeTenderCancelled
     *         whether the whole tender is cancelled (or just some of its lots)
     *
     * @return this instance for chaining
     */

    public final ParsedTender setIsWholeTenderCancelled(final String isWholeTenderCancelled) {
        this.isWholeTenderCancelled = isWholeTenderCancelled;
        return this;
    }

    /**
     * @return the enquiryDeadline
     */

    
    public final String getEnquiryDeadline() {
        return enquiryDeadline;
    }

    /**
     * @param enquiryDeadline
     *         the enquiryDeadline to set
     *
     * @return this instance for chaining
     */

    public final ParsedTender setEnquiryDeadline(final String enquiryDeadline) {
        this.enquiryDeadline = enquiryDeadline;
        return this;
    }

    /**
     * @return the awardDeadline
     */

    
    public final String getAwardDeadline() {
        return awardDeadline;
    }

    /**
     * @param awardDeadline
     *         the awardDeadline to set
     *
     * @return this instance for chaining
     */

    public final ParsedTender setAwardDeadline(final String awardDeadline) {
        this.awardDeadline = awardDeadline;
        return this;
    }

    /**
     * @return the awardDeadlineDuration
     */
    
    public final String getAwardDeadlineDuration() {
        return awardDeadlineDuration;
    }

    /**
     * @param awardDeadlineDuration
     *         the awardDeadlineDuration to set
     *
     * @return this instance for chaining
     */
    public final ParsedTender setAwardDeadlineDuration(final String awardDeadlineDuration) {
        this.awardDeadlineDuration = awardDeadlineDuration;
        return this;
    }

    /**
     * @return the finalPrice
     */

    
    public final ParsedPrice getFinalPrice() {
        return finalPrice;
    }

    /**
     * @param finalPrice
     *         the finalPrice to set
     *
     * @return this instance for chaining
     */

    public final ParsedTender setFinalPrice(final ParsedPrice finalPrice) {
        this.finalPrice = finalPrice;
        return this;
    }

    /**
     * @return the eligibleBidLanguages
     */

    
    public final List<String> getEligibleBidLanguages() {
        return eligibleBidLanguages;
    }

    /**
     * @param eligibleBidLanguages
     *         the eligibleBidLanguages to set
     *
     * @return this instance for chaining
     */

    public final ParsedTender setEligibleBidLanguages(final List<String> eligibleBidLanguages) {
        this.eligibleBidLanguages = eligibleBidLanguages;
        return this;
    }

    /**
     * @param eligibleBidLanguage
     *         the eligibleBidLanguage to be add
     *
     * @return this instance for chaining
     */
    public final ParsedTender addEligibleBidLanguage(final String eligibleBidLanguage) {
        if (eligibleBidLanguage != null) {
            if (getEligibleBidLanguages() == null) {
                setEligibleBidLanguages(new ArrayList<>());
            }

            this.eligibleBidLanguages.add(eligibleBidLanguage);
        }

        return this;
    }

    /**
     * @return the isEInvoiceAccepted
     */

    
    public final String getIsEInvoiceAccepted() {
        return isEInvoiceAccepted;
    }

    /**
     * @param isEInvoiceAccepted
     *         the isEInvoiceAccepted to set
     *
     * @return this instance for chaining
     */

    public final ParsedTender setIsEInvoiceAccepted(final String isEInvoiceAccepted) {
        this.isEInvoiceAccepted = isEInvoiceAccepted;
        return this;
    }

    /**
     * @return the corrections
     */

    
    public final List<ParsedCorrigendum> getCorrections() {
        return corrections;
    }

    /**
     * @param corrections
     *         the corrections to set
     *
     * @return this instance for chaining
     */

    public final ParsedTender setCorrections(final List<ParsedCorrigendum> corrections) {
        this.corrections = corrections;
        return this;
    }

    /**
     * Gets modificationReason.
     *
     * @return value of modificationReason
     */
    
    public final String getModificationReason() {
        return modificationReason;
    }

    /**
     * Sets modificationReason.
     *
     * @param modificationReason
     *         the modificationReason to set
     *
     * @return this instance for chaining
     */
    public final ParsedTender setModificationReason(final String modificationReason) {
        this.modificationReason = modificationReason;
        return this;
    }

    /**
     * Gets modificationReasonDescription.
     *
     * @return value of modificationReasonDescription
     */
    
    public final String getModificationReasonDescription() {
        return modificationReasonDescription;
    }

    /**
     * Sets modificationReasonDescription.
     *
     * @param modificationReasonDescription
     *         the modificationReasonDescription to set
     *
     * @return this instance for chaining
     */
    public final ParsedTender setModificationReasonDescription(final String modificationReasonDescription) {
        this.modificationReasonDescription = modificationReasonDescription;
        return this;
    }

    /**
     * Gets additionalInfo.
     *
     * @return value of additionalInfo
     */
    public final String getAdditionalInfo() {
        return additionalInfo;
    }

    /**
     * Sets additionalInfo.
     *
     * @param additionalInfo
     *         the additionalInfo to set
     *
     * @return this instance for chaining
     */
    public final ParsedTender setAdditionalInfo(final String additionalInfo) {
        this.additionalInfo = additionalInfo;
        return this;
    }

    /**
     * Gets country.
     *
     * @return value of country
     */
    public final String getCountry() {
        return country;
    }

    /**
     * Sets countyr.
     *
     * @param country
     *         the country of origin to set
     *
     * @return this instance for chaining
     */
    public final ParsedTender setCountry(final String country) {
        this.country = country;
        return this;
    }

    /**
     * @return the createdRaw
     */
    public final LocalDateTime getCreatedRaw() {
        return createdRaw;
    }

    /**
     * @param createdRaw created date of raw object
     */
    public final void setCreatedRaw(final LocalDateTime createdRaw) {
        this.createdRaw = createdRaw;
    }

    /**
     * Adds corrigendum to the list of corrections.
     *
     * @param corrigendum
     *         new corrigendum to be added
     *
     * @return this instance for chaining
     */

    public final ParsedTender addCorrigendum(final ParsedCorrigendum corrigendum) {
        if (corrigendum != null) {
            if (getCorrections() == null) {
                setCorrections(new ArrayList<>());
            }
            this.corrections.add(corrigendum);
        }

        return this;
    }

    /**
     * Adds all corrections to the list of corrections.
     *
     * @param newCorrections
     *         new corrigendum to be added
     *
     * @return this instance for chaining
     */

    public final ParsedTender addCorrections(final List<ParsedCorrigendum> newCorrections) {
        if (newCorrections != null && !newCorrections.isEmpty()) {
            if (getCorrections() == null) {
                setCorrections(new ArrayList<>());
            }
            this.corrections.addAll(newCorrections);
        }

        return this;
    }

    /**
     * Adds publication to the list of publications or create a new list with given publication if none exists.
     *
     * @param publication
     *         new publication to be added
     *
     * @return this instance for chaining
     */

    public final ParsedTender addPublication(final ParsedPublication publication) {
        if (publication != null) {
            if (getPublications() == null) {
                setPublications(new ArrayList<>());
            }
            this.publications.add(publication);
        }
        return this;
    }

    /**
     * Adds all publications to the list of publications or create a new list with given list of publications if none
     * exists.
     *
     * @param newPublications
     *         new publications to be added
     *
     * @return this instance for chaining
     */

    public final ParsedTender addPublications(final List<ParsedPublication> newPublications) {
        if (newPublications != null) {
            if (getPublications() == null) {
                setPublications(new ArrayList<>());
            }
            this.publications.addAll(newPublications);
        }
        return this;
    }

    /**
     * Adds administrator to the list of administrator or create a new list with given administrator if none exists.
     *
     * @param administrator
     *         new administrator to be added
     *
     * @return this instance for chaining
     */

    public final ParsedTender addAdministrator(final ParsedBody administrator) {
        if (administrator != null) {
            if (getAdministrators() == null) {
                setAdministrators(new ArrayList<>());
            }

            this.administrators.add(administrator);
        }
        return this;
    }

    /**
     * Adds buyer to the list of buyers or create a new list with given buyer if none exists.
     *
     * @param buyer
     *         new buyer to be added
     *
     * @return this instance for chaining
     */

    public final ParsedTender addBuyer(final ParsedBody buyer) {
        if (buyer != null) {
            if (getBuyers() == null) {
                setBuyers(new ArrayList<>());
            }

            this.buyers.add(buyer);
        }
        return this;
    }

    /**
     * Adds reason to the list of NPWP reasons or create a new list with given reason if none exists.
     *
     * @param npwpReason
     *         new reason to be added
     *
     * @return this instance for chaining
     */

    public final ParsedTender addNpwpReason(final String npwpReason) {
        if (npwpReason != null) {
            if (getNpwpReasons() == null) {
                setNpwpReasons(new ArrayList<>());
            }

            this.npwpReasons.add(npwpReason);
        }
        return this;
    }

    /**
     * Adds Lot to the list of Lots or create a new list with given Lot if none exists.
     *
     * @param lot
     *         the lot to add
     *
     * @return this instance for chaining
     */

    public final ParsedTender addLot(final ParsedTenderLot lot) {
        if (lot != null) {
            if (getLots() == null) {
                setLots(new ArrayList<>());
            }

            this.lots.add(lot);
        }
        return this;
    }

    /**
     * Adds Lots to the list of Lots or creates a new list with given Lots if none exists.
     *
     * @param newLots
     *         the lots to add
     *
     * @return this instance for chaining
     */

    public final ParsedTender addLots(final List<ParsedTenderLot> newLots) {
        if (newLots != null && !newLots.isEmpty()) {
            if (getLots() == null) {
                setLots(new ArrayList<>());
            }

            this.lots.addAll(newLots);
        }
        return this;
    }
}
