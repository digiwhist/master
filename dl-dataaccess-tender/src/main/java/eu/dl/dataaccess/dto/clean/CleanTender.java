package eu.dl.dataaccess.dto.clean;

import static eu.dl.dataaccess.utils.ClassUtils.removeNonsenses;

import java.net.URL;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnore;

import eu.dl.dataaccess.annotation.Transformable;
import eu.dl.dataaccess.dto.codetables.NpwpReason;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.codetables.TenderSize;
import eu.dl.dataaccess.dto.codetables.TenderSupplyType;
import eu.dl.dataaccess.dto.generic.Address;
import eu.dl.dataaccess.dto.generic.Corrigendum;
import eu.dl.dataaccess.dto.generic.Document;
import eu.dl.dataaccess.dto.generic.Price;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.utils.ValidationUtils;

/**
 * Public Contract. Contains full info about single contract.
 */
@Transformable
public class CleanTender extends BaseCleanTenderLot<CleanTender> implements Cleanable {

    /**
     * Reference number given to contract by buyer.
     */
    private String buyerAssignedId;

    /**
     * Type of procedure.
     */
    private TenderProcedureType procedureType;

    /**
     * Type of procedure - unstructured.
     */
    private String nationalProcedureType;

    /**
     * Accelerated procedure.
     */
    private Boolean isAcceleratedProcedure;

    /**
     * Reason for accelerated procedure.
     */
    private String acceleratedProcedureJustification;

    /**
     * Maximum number of bids, typically used in Restricted or Negotiated with
     * publication procedure.
     */
    private Integer maxBidsCount;

    /**
     * Type of supplies (services/goods/construction).
     */
    private TenderSupplyType supplyType;

    /**
     * Tender size (below/above the threshold).
     */
    private TenderSize size;

    /**
     * Date until which bids need to be submitted (do not confuse with
     * application deadline for Restricted procedure type).
     */
    private LocalDateTime bidDeadline;

    /**
     * Date until tender documents or additional information are provided.
     */
    private LocalDateTime documentsDeadline;

    /**
     * Price is asked for tender documents provision.
     */
    private Boolean documentsPayable;

    /**
     * Price of tender documents.
     */
    private Price documentsPrice;

    /**
     * Location where tender documentation can be obtained (url or name of
     * organisation).
     */
    private Address documentsLocation;

    /**
     * Free/restricted access to documents.
     */
    private Boolean isDocumentsAccessRestricted;

    /**
     * Tender is awarded by central purchasing authority.
     */
    private Boolean isCentralProcurement;

    /**
     * Tender involves joint procurement.
     */
    private Boolean isJointProcurement;

    /**
     * Identification of buyer(contracting authority) or other purchasing body.
     */
    private List<CleanBody> buyers;

    /**
     * The purchase is being made for someone else. e.g. city purchases on
     * behalf of one of its schools.
     */
    private Boolean isOnBehalfOf;

    /**
     * The purchase is being made for someone else. e.g. city purchases on
     * behalf of one of its schools.
     */
    private List<CleanBody> onBehalfOf;

    /**
     * Body from whom further information can be obtained.
     */
    private CleanBody furtherInformationProvider;

    /**
     * Body from whom specifications and additional documents can be obtained.
     */
    private CleanBody specificationsProvider;

    /**
     * Body to whom tenders/requests to participate must be sent.
     */
    private CleanBody bidsRecipient;

    /**
     * List of relevant publications.
     */
    private List<Publication> publications;

    /**
     * Data on external contract administrators, to whom the procedure
     * administration was outsourced (in case it was).
     */
    private List<CleanBody> administrators;

    /**
     * Data on external supervisors, to whom the supervision over the contract
     * fulfillment was outsourced (in case it was).
     */
    private List<CleanBody> supervisors;

    /**
     * Data on external body, to whom the preparation of tender documents was
     * outsourced (in case it was).
     */
    private CleanBody specificationsCreator;

    /**
     * Tender is divided into lots.
     */
    private Boolean hasLots;

    /**
     * List of contract lots.
     */
    private List<CleanTenderLot> lots;

    /**
     * List of candidates who expressed interest in contract (typically in
     * Restricted procedure or Negotiated procedure with publication).
     */
    private List<CleanBody> candidates;

    /**
     * List of potential suppliers actively approached or consulted by
     * contracting authority with information on contract.
     */
    private List<CleanBody> approachedBidders;

    /**
     * List of documents relevant to contract (tender documentation, protocol on
     * bids evaluation etc).
     */
    private List<Document> documents;

    /**
     * Links to information in case, there has been any form of court proceeding
     * related to contract (commenced rather than consluded necessarily).
     */
    private List<URL> courtProceedings;

    /**
     * Links to information in case, there has been a court intervention into
     * the tender/contract.
     */
    private List<URL> courtInterventions;

    /**
     * Reasons for use of negotiated procedure without publication.
     */
    private List<NpwpReason> npwpReasons;

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
    private Boolean isWholeTenderCancelled;

    /**
     * Deadline date for sending enquiries to the subject.
     */
    private LocalDate enquiryDeadline;

    /**
     * Deadline for awarding the contract (how long are bidders bound by their
     * bids).
     */
    private LocalDate awardDeadline;

    /**
     * How long are bidders bound by their bids (from the date stated for receipt of tender).
     */
    private Integer awardDeadlineDuration;

    /**
     * Final value of contract.
     */
    private Price finalPrice;

    /**
     * Languages (ISO 639 codes) in which tenders or requests to participate may
     * be submitted.
     */
    private List<String> eligibleBidLanguages;

    /**
     * Electronic invoicing will be accepted.
     */
    private Boolean isEInvoiceAccepted;

    /**
     * List of corrections.
     */
    private List<Corrigendum> corrections;

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
     * @param newBuyerAssignedId
     *         the buyerAssignedId to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setBuyerAssignedId(final String newBuyerAssignedId) {
        this.buyerAssignedId = newBuyerAssignedId;
        return this;
    }

    /**
     * @return the procedureType
     */
    
    public final TenderProcedureType getProcedureType() {
        return procedureType;
    }

    /**
     * @param newProcedureType
     *         the procedureType to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setProcedureType(final TenderProcedureType newProcedureType) {
        this.procedureType = newProcedureType;
        return this;
    }

    /**
     * @return the nationalProcedureType
     */
    
    public final String getNationalProcedureType() {
        return nationalProcedureType;
    }

    /**
     * @param newNationalProcedureType
     *         the nationalProcedureType to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setNationalProcedureType(final String newNationalProcedureType) {
        this.nationalProcedureType = newNationalProcedureType;
        return this;
    }

    /**
     * @return is the procedure accelerated
     */
    
    public final Boolean getIsAcceleratedProcedure() {
        return isAcceleratedProcedure;
    }

    /**
     * @param newIsAcceleratedProcedure
     *         is the procedure accelerated
     *
     * @return this instance for chaining
     */
    public final CleanTender setIsAcceleratedProcedure(final Boolean newIsAcceleratedProcedure) {
        this.isAcceleratedProcedure = newIsAcceleratedProcedure;
        return this;
    }

    /**
     * @return reason for the accelerated procedure
     */
    
    public final String getAcceleratedProcedureJustification() {
        return acceleratedProcedureJustification;
    }

    /**
     * @param newAcceleratedProcedureJustification
     *         reason for the accelerated procedure
     *
     * @return this instance for chaining
     */
    public final CleanTender setAcceleratedProcedureJustification(final String newAcceleratedProcedureJustification) {
        this.acceleratedProcedureJustification = newAcceleratedProcedureJustification;
        return this;
    }

    /**
     * @return the maxBidsCount
     */
    
    public final Integer getMaxBidsCount() {
        return maxBidsCount;
    }

    /**
     * @param newMaxBidsCount
     *         the maxBidsCount to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setMaxBidsCount(final Integer newMaxBidsCount) {
        this.maxBidsCount = newMaxBidsCount;
        return this;
    }

    /**
     * @return the supplyType
     */
    
    public final TenderSupplyType getSupplyType() {
        return supplyType;
    }

    /**
     * @param newSupplyType
     *         the supplyType to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setSupplyType(final TenderSupplyType newSupplyType) {
        this.supplyType = newSupplyType;
        return this;
    }

    /**
     * @return the size
     */
    
    public final TenderSize getSize() {
        return size;
    }

    /**
     * @param newSize
     *         the size to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setSize(final TenderSize newSize) {
        this.size = newSize;
        return this;
    }

    /**
     * @return the bidDeadline
     */
    
    public final LocalDateTime getBidDeadline() {
        return bidDeadline;
    }

    /**
     * @param newBidDeadline
     *         the bidDeadline to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setBidDeadline(final LocalDateTime newBidDeadline) {
        this.bidDeadline = newBidDeadline;
        return this;
    }

    /**
     * @return the documentsDeadline
     */
    
    public final LocalDateTime getDocumentsDeadline() {
        return documentsDeadline;
    }

    /**
     * @param newDocumentsDeadline
     *         the documentsDeadline to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setDocumentsDeadline(final LocalDateTime newDocumentsDeadline) {
        this.documentsDeadline = newDocumentsDeadline;
        return this;
    }

    /**
     * @return the documentsPayable
     */
    
    public final Boolean getDocumentsPayable() {
        return documentsPayable;
    }

    /**
     * @param newDocumentsPayable
     *         the documentsPayable to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setDocumentsPayable(final Boolean newDocumentsPayable) {
        this.documentsPayable = newDocumentsPayable;
        return this;
    }

    /**
     * @return the documentsPrice
     */
    
    public final Price getDocumentsPrice() {
        return documentsPrice;
    }

    /**
     * @param newDocumentsPrice
     *         the documentsPrice to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setDocumentsPrice(final Price newDocumentsPrice) {
        this.documentsPrice = newDocumentsPrice;
        return this;
    }

    /**
     * @return the documentsLocation
     */
    
    public final Address getDocumentsLocation() {
        return documentsLocation;
    }

    /**
     * @param newDocumentsLocation
     *         the documentsLocation to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setDocumentsLocation(final Address newDocumentsLocation) {
        this.documentsLocation = newDocumentsLocation;
        return this;
    }

    /**
     * @return true if the access to documents is free, false for restricted
     * access
     */
    
    public final Boolean getIsDocumentsAccessRestricted() {
        return isDocumentsAccessRestricted;
    }

    /**
     * @param newIsDocumentsAccessRestricted
     *         boolean whether the access to documents is free
     *
     * @return this instance for chaining
     */
    public final CleanTender setIsDocumentsAccessRestricted(final Boolean newIsDocumentsAccessRestricted) {
        this.isDocumentsAccessRestricted = newIsDocumentsAccessRestricted;
        return this;
    }

    /**
     * @return the isCentralProcurement
     */
    
    public final Boolean getIsCentralProcurement() {
        return isCentralProcurement;
    }

    /**
     * @param newIsCentralProcurement
     *         the isCentralProcurement to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setIsCentralProcurement(final Boolean newIsCentralProcurement) {
        this.isCentralProcurement = newIsCentralProcurement;
        return this;
    }

    /**
     * @return the isJointProcurement
     */
    
    public final Boolean getIsJointProcurement() {
        return isJointProcurement;
    }

    /**
     * @param newIsJointProcurement
     *         the isJointProcurement to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setIsJointProcurement(final Boolean newIsJointProcurement) {
        this.isJointProcurement = newIsJointProcurement;
        return this;
    }

    /**
     * @return the buyers
     */
    
    public final List<CleanBody> getBuyers() {
        return buyers;
    }

    /**
     * @param newBuyers
     *         the buyers to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setBuyers(final List<CleanBody> newBuyers) {
        this.buyers = newBuyers;
        return this;
    }

    /**
     * Adds an buyer to the buyers list.
     *
     * @param buyer
     *         buyer
     *
     * @return this instance for chaining
     */
    public final CleanTender addBuyer(final CleanBody buyer) {
        if (buyer != null) {
            if (this.buyers == null) {
                this.buyers = new ArrayList<>();
            }

            this.buyers.add(buyer);
        }

        return this;
    }

    /**
     * @return true if the purchase is being made for someone else
     */
    
    public final Boolean getIsOnBehalfOf() {
        return isOnBehalfOf;
    }

    /**
     * @param newIsOnBehalfOf
     *         booleaN whether the purchase is being made for someone else
     *
     * @return this instance for chaining
     */
    public final CleanTender setIsOnBehalfOf(final Boolean newIsOnBehalfOf) {
        this.isOnBehalfOf = newIsOnBehalfOf;
        return this;
    }

    /**
     * @return the list of onBehalfOf
     */
    
    public final List<CleanBody> getOnBehalfOf() {
        return onBehalfOf;
    }

    /**
     * @param newOnBehalfOf
     *         the list of onBehalfOf to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setOnBehalfOf(final List<CleanBody> newOnBehalfOf) {
        this.onBehalfOf = newOnBehalfOf;
        return this;
    }

    /**
     * @param newOnBehalfOf
     *         the onBehalfOf to add
     *
     * @return this instance for chaining
     */
    public final CleanTender addOnBehalfOf(final CleanBody newOnBehalfOf) {
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
    
    public final CleanBody getFurtherInformationProvider() {
        return furtherInformationProvider;
    }

    /**
     * @param newFurtherInformationProvider
     *         body from whom further information can be obtained *
     *
     * @return this instance for chaining
     */
    public final CleanTender setFurtherInformationProvider(final CleanBody newFurtherInformationProvider) {
        this.furtherInformationProvider = newFurtherInformationProvider;
        return this;
    }

    /**
     * @return body from whom specifications and additional documents can be
     * obtained
     */
    
    public final CleanBody getSpecificationsProvider() {
        return specificationsProvider;
    }

    /**
     * @param newSpecificationsProvider
     *         body from whom specifications and additional documents can be
     *         obtained
     *
     * @return this instance for chaining
     */
    public final CleanTender setSpecificationsProvider(final CleanBody newSpecificationsProvider) {
        this.specificationsProvider = newSpecificationsProvider;
        return this;
    }

    /**
     * @return body to whom tenders/requests to participate must be sent
     */
    
    public final CleanBody getBidsRecipient() {
        return bidsRecipient;
    }

    /**
     * @param newBidsRecipient
     *         body to whom tenders/requests to participate must be sent
     *
     * @return this instance for chaining
     */
    public final CleanTender setBidsRecipient(final CleanBody newBidsRecipient) {
        this.bidsRecipient = newBidsRecipient;
        return this;
    }

    /**
     * @return the public finalations
     */
    
    public final List<Publication> getPublications() {
        return publications;
    }

    /**
     * @param newPublications
     *         the publications to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setPublications(final List<Publication> newPublications) {
        this.publications = newPublications;
        return this;
    }

    /**
     * @return the administrators
     */
    
    public final List<CleanBody> getAdministrators() {
        return administrators;
    }

    /**
     * @param newAdministrators
     *         the administrators to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setAdministrators(final List<CleanBody> newAdministrators) {
        this.administrators = newAdministrators;
        return this;
    }

    /**
     * @return the supervisors
     */
    
    public final List<CleanBody> getSupervisors() {
        return supervisors;
    }

    /**
     * @param newSupervisors
     *         the supervisors to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setSupervisors(final List<CleanBody> newSupervisors) {
        this.supervisors = newSupervisors;
        return this;
    }

    /**
     * @return the specificationsCreator
     */
    
    public final CleanBody getSpecificationsCreator() {
        return specificationsCreator;
    }

    /**
     * @param newSpecificationsCreator
     *         the specificationsCreator to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setSpecificationsCreator(final CleanBody newSpecificationsCreator) {
        this.specificationsCreator = newSpecificationsCreator;
        return this;
    }

    /**
     * @return is tender divided into lots
     */
    
    public final Boolean getHasLots() {
        return hasLots;
    }

    /**
     * @param newHasLots
     *         is tender divided into lots
     *
     * @return this instance for chaining
     */
    public final CleanTender setHasLots(final Boolean newHasLots) {
        this.hasLots = newHasLots;
        return this;
    }

    /**
     * @return the lots
     */
    
    public final List<CleanTenderLot> getLots() {
        return lots;
    }

    /**
     * @param newLots
     *         the lots to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setLots(final List<CleanTenderLot> newLots) {
        this.lots = newLots;
        return this;
    }

    /**
     * Adds lot to the lots list.
     *
     * @param lot
     *         lot
     *
     * @return this instance for chaining
     */
    public final CleanTender addLot(final CleanTenderLot lot) {
        if (lot != null) {
            if (this.lots == null) {
                setLots(new ArrayList<>());
            }

            this.lots.add(lot);
        }

        return this;
    }

    /**
     * @return the candidates
     */
    
    public final List<CleanBody> getCandidates() {
        return candidates;
    }

    /**
     * @param newCandidates
     *         the candidates to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setCandidates(final List<CleanBody> newCandidates) {
        this.candidates = newCandidates;
        return this;
    }

    /**
     * @return the approachedBidders
     */
    
    public final List<CleanBody> getApproachedBidders() {
        return approachedBidders;
    }

    /**
     * @param newApproachedBidders
     *         the approachedBidders to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setApproachedBidders(final List<CleanBody> newApproachedBidders) {
        this.approachedBidders = newApproachedBidders;
        return this;
    }

    /**
     * @return the documents
     */
    
    public final List<Document> getDocuments() {
        return documents;
    }

    /**
     * @param newDocuments
     *         the documents to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setDocuments(final List<Document> newDocuments) {
        this.documents = newDocuments;
        return this;
    }

    /**
     * @return the courtProceedings
     */
    
    public final List<URL> getCourtProceedings() {
        return courtProceedings;
    }

    /**
     * @param newCourtProceedings
     *         the courtProceedings to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setCourtProceedings(final List<URL> newCourtProceedings) {
        this.courtProceedings = newCourtProceedings;
        return this;
    }

    /**
     * @return the courtInterventions
     */
    
    public final List<URL> getCourtInterventions() {
        return courtInterventions;
    }

    /**
     * @param newCourtInterventions
     *         the courtInterventions to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setCourtInterventions(final List<URL> newCourtInterventions) {
        this.courtInterventions = newCourtInterventions;
        return this;
    }

    /**
     * @return the npwpReasons
     */
    
    public final List<NpwpReason> getNpwpReasons() {
        return npwpReasons;
    }

    /**
     * @param newNpwpReasons
     *         the npwpReasons to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setNpwpReasons(final List<NpwpReason> newNpwpReasons) {
        this.npwpReasons = newNpwpReasons;
        return this;
    }

    /**
     * @return the deposits
     */
    
    public final String getDeposits() {
        return deposits;
    }

    /**
     * @param newDeposits
     *         the deposits to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setDeposits(final String newDeposits) {
        this.deposits = newDeposits;
        return this;
    }

    /**
     * @return the personalRequirements
     */
    
    public final String getPersonalRequirements() {
        return personalRequirements;
    }

    /**
     * @param newPersonalRequirements
     *         the personalRequirements to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setPersonalRequirements(final String newPersonalRequirements) {
        this.personalRequirements = newPersonalRequirements;
        return this;
    }

    /**
     * @return the economicRequirements
     */
    
    public final String getEconomicRequirements() {
        return economicRequirements;
    }

    /**
     * @param newEconomicRequirements
     *         the economicRequirements to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setEconomicRequirements(final String newEconomicRequirements) {
        this.economicRequirements = newEconomicRequirements;
        return this;
    }

    /**
     * @return the technicalRequirements
     */
    
    public final String getTechnicalRequirements() {
        return technicalRequirements;
    }

    /**
     * @param newTechnicalRequirements
     *         the technicalRequirements to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setTechnicalRequirements(final String newTechnicalRequirements) {
        this.technicalRequirements = newTechnicalRequirements;
        return this;
    }

    /**
     * @return the appealBodyName
     */
    
    public final String getAppealBodyName() {
        return appealBodyName;
    }

    /**
     * @param newAppealBodyName
     *         the appealBodyName to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setAppealBodyName(final String newAppealBodyName) {
        this.appealBodyName = newAppealBodyName;
        return this;
    }

    /**
     * @return the mediationBodyName
     */
    
    public final String getMediationBodyName() {
        return mediationBodyName;
    }

    /**
     * @param newMediationBodyName
     *         the mediationBodyName to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setMediationBodyName(final String newMediationBodyName) {
        this.mediationBodyName = newMediationBodyName;
        return this;
    }

    /**
     * @return justification for framework agreement going over 4 years
     */
    
    public final String getExcessiveFrameworkAgreementJustification() {
        return excessiveFrameworkAgreementJustification;
    }

    /**
     * @param newExcessiveFrameworkAgreementJustification
     *         justification for framework agreement going over 4 years
     *
     * @return this instance for chaining
     */
    public final CleanTender setExcessiveFrameworkAgreementJustification(
            final String newExcessiveFrameworkAgreementJustification) {
        this.excessiveFrameworkAgreementJustification = newExcessiveFrameworkAgreementJustification;
        return this;
    }

    /**
     * @return true if the whole tender is cancelled, false if just some of its
     * lots
     */
    
    public final Boolean getIsWholeTenderCancelled() {
        return isWholeTenderCancelled;
    }

    /**
     * @param newWholeTenderCancelled
     *         whether the whole tender is cancelled (or just some of its
     *         lots)
     *
     * @return this instance for chaining
     */
    public final CleanTender setIsWholeTenderCancelled(final Boolean newWholeTenderCancelled) {
        this.isWholeTenderCancelled = newWholeTenderCancelled;
        return this;
    }

    /**
     * @return the enquiryDeadline
     */
    
    public final LocalDate getEnquiryDeadline() {
        return enquiryDeadline;
    }

    /**
     * @param newEnquiryDeadline
     *         the enquiryDeadline to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setEnquiryDeadline(final LocalDate newEnquiryDeadline) {
        this.enquiryDeadline = newEnquiryDeadline;
        return this;
    }

    /**
     * @return the awardDeadline
     */
    
    public final LocalDate getAwardDeadline() {
        return awardDeadline;
    }

    /**
     * @param newAwardDeadline
     *         the awardDeadline to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setAwardDeadline(final LocalDate newAwardDeadline) {
        this.awardDeadline = newAwardDeadline;
        return this;
    }

    /**
     * @return the awardDeadlineDuration
     */
    
    public final Integer getAwardDeadlineDuration() {
        return awardDeadlineDuration;
    }

    /**
     * @param newAwardDeadlineDuration
     *         the awardDeadlineDuration to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setAwardDeadlineDuration(final Integer newAwardDeadlineDuration) {
        this.awardDeadlineDuration = newAwardDeadlineDuration;
        return this;
    }

    /**
     * @return the finalPrice
     */
    
    public final Price getFinalPrice() {
        return finalPrice;
    }

    /**
     * @param newFinalPrice
     *         the finalPrice to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setFinalPrice(final Price newFinalPrice) {
        this.finalPrice = newFinalPrice;
        return this;
    }

    /**
     * @return the eligibleBidLanguages
     */
    
    public final List<String> getEligibleBidLanguages() {
        return eligibleBidLanguages;
    }

    /**
     * @param newEligibleBidLanguages
     *         the eligibleBidLanguages to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setEligibleBidLanguages(final List<String> newEligibleBidLanguages) {
        this.eligibleBidLanguages = newEligibleBidLanguages;
        return this;
    }

    /**
     * @return the isEInvoiceAccepted
     */
    
    public final Boolean getIsEInvoiceAccepted() {
        return isEInvoiceAccepted;
    }

    /**
     * @param newIsEInvoiceAccepted
     *         the isEInvoiceAccepted to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setIsEInvoiceAccepted(final Boolean newIsEInvoiceAccepted) {
        this.isEInvoiceAccepted = newIsEInvoiceAccepted;
        return this;
    }

    /**
     * @return the corrections
     */
    
    public final List<Corrigendum> getCorrections() {
        return corrections;
    }

    /**
     * @param newCorrections
     *         the corrections to set
     *
     * @return this instance for chaining
     */
    public final CleanTender setCorrections(final List<Corrigendum> newCorrections) {
        this.corrections = newCorrections;
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
    public final CleanTender setModificationReason(final String modificationReason) {
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
    public final CleanTender setModificationReasonDescription(final String modificationReasonDescription) {
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
    public final CleanTender setAdditionalInfo(final String additionalInfo) {
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
    public final CleanTender setCountry(final String country) {
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

    @Override
    
    @JsonIgnore
    @Transformable
    public final CleanTender getValid() {
        setAddressOfImplementation(removeNonsenses(addressOfImplementation));
        setAdministrators(ValidationUtils.getValid(administrators));
        setApproachedBidders(ValidationUtils.getValid(approachedBidders));
        setAwardCriteria(ValidationUtils.getValid(awardCriteria));
        setBidsRecipient(removeNonsenses(bidsRecipient));
        setBuyers(ValidationUtils.getValid(buyers));
        setCandidates(ValidationUtils.getValid(candidates));
        setCorrections(ValidationUtils.getValid(corrections));
        setCourtInterventions(ValidationUtils.getValid(courtInterventions));
        setCourtProceedings(ValidationUtils.getValid(courtProceedings));
        setCpvs(ValidationUtils.getValid(cpvs));
        setDocuments(ValidationUtils.getValid(documents));
        setDocumentsLocation(removeNonsenses(documentsLocation));
        setDocumentsPrice(removeNonsenses(documentsPrice));
        setEligibleBidLanguages(ValidationUtils.getValid(eligibleBidLanguages));
        setEstimatedPrice(removeNonsenses(estimatedPrice));
        setFinalPrice(removeNonsenses(finalPrice));
        setFundings(ValidationUtils.getValid(fundings));
        setFurtherInformationProvider(removeNonsenses(furtherInformationProvider));
        setLots(ValidationUtils.getValid(lots));
        setNpwpReasons(ValidationUtils.getValid(npwpReasons));
        setOnBehalfOf(ValidationUtils.getValid(onBehalfOf));
        setPublications(ValidationUtils.getValid(publications));
        setSpecificationsCreator(removeNonsenses(specificationsCreator));
        setSpecificationsProvider(removeNonsenses(specificationsProvider));
        setSupervisors(ValidationUtils.getValid(supervisors));

        // for CleanTender is not necessary to check whether is empty, we assume that each tender has at least one
        // not null parameter.
        return this;
    }
}
