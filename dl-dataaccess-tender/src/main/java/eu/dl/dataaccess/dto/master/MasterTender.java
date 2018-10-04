package eu.dl.dataaccess.dto.master;

import java.net.URL;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;


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
import eu.dl.dataaccess.dto.indicator.Indicator;


/**
 * Public Contract. Contains full info about single contract.
 */
@Transformable
public class MasterTender extends BaseMasterTenderLot<MasterTender> implements Masterable {

    /**
     * Constructs empty master tender.
     */
    public MasterTender() {
        super();
    }

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
    private List<MasterBody> buyers;

    /**
     * The purchase is being made for someone else. e.g. city purchases on
     * behalf of one of its schools.
     */
    private Boolean isOnBehalfOf;

    /**
     * The purchase is being made for someone else. e.g. city purchases on
     * behalf of one of its schools.
     */
    private List<MasterBody> onBehalfOf;

    /**
     * Body from whom further information can be obtained.
     */
    private MasterBody furtherInformationProvider;

    /**
     * Body from whom specifications and additional documents can be obtained.
     */
    private MasterBody specificationsProvider;

    /**
     * Body to whom tenders/requests to participate must be sent.
     */
    private MasterBody bidsRecipient;

    /**
     * List of relevant publications.
     */
    private List<Publication> publications;

    /**
     * Data on external contract administrators, to whom the procedure
     * administration was outsourced (in case it was).
     */
    private List<MasterBody> administrators;

    /**
     * Data on external supervisors, to whom the supervision over the contract
     * fulfillment was outsourced (in case it was).
     */
    private List<MasterBody> supervisors;

    /**
     * Data on external body, to whom the preparation of tender documents was
     * outsourced (in case it was).
     */
    private MasterBody specificationsCreator;

    /**
     * Tender is divided into lots.
     */
    private Boolean hasLots;

    /**
     * List of contract lots.
     */
    private List<MasterTenderLot> lots;

    /**
     * List of candidates who expressed interest in contract (typically in
     * Restricted procedure or Negotiated procedure with publication).
     */
    private List<MasterBody> candidates;

    /**
     * List of potential suppliers actively approached or consulted by
     * contracting authority with information on contract.
     */
    private List<MasterBody> approachedBidders;

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
     * Country of origin.
     */
    private String country;

    /**
     * Additional info.
     */
    private String additionalInfo;

    private String hash;

    private String groupId;

    private String masterBy;
    
    /*
     * Indicators related to this tender.
     */
    private List<Indicator> indicators;

    /**
     * Digiwhist price.
     */
    private Price digiwhistPrice;

    /**
     * Gets the buyer assigned id.
     *
     * @return the buyerAssignedId
     */
    
    public final String getBuyerAssignedId() {
        return buyerAssignedId;
    }

    /**
     * Sets the buyer assigned id.
     *
     * @param newBuyerAssignedId
     *         the buyerAssignedId to set
     *
     * @return this instance for chaining
     */
    public final MasterTender setBuyerAssignedId(final String newBuyerAssignedId) {
        this.buyerAssignedId = newBuyerAssignedId;
        return this;
    }

    /**
     * Gets the procedure type.
     *
     * @return the procedureType
     */
    
    public final TenderProcedureType getProcedureType() {
        return procedureType;
    }

    /**
     * Sets the procedure type.
     *
     * @param newProcedureType
     *         the procedureType to set
     *
     * @return this instance for chaining
     */
    public final MasterTender setProcedureType(final TenderProcedureType newProcedureType) {
        this.procedureType = newProcedureType;
        return this;
    }

    /**
     * Gets the national procedure type.
     *
     * @return the nationalProcedureType
     */
    
    public final String getNationalProcedureType() {
        return nationalProcedureType;
    }

    /**
     * Sets the national procedure type.
     *
     * @param newNationalProcedureType
     *         the nationalProcedureType to set
     *
     * @return this instance for chaining
     */
    public final MasterTender setNationalProcedureType(final String newNationalProcedureType) {
        this.nationalProcedureType = newNationalProcedureType;
        return this;
    }

    /**
     * Gets the checks if is accelerated procedure.
     *
     * @return is the procedure accelerated
     */
    
    public final Boolean getIsAcceleratedProcedure() {
        return isAcceleratedProcedure;
    }

    /**
     * Sets the is accelerated procedure.
     *
     * @param newIsAcceleratedProcedure
     *         is the procedure accelerated
     *
     * @return this instance for chaining
     */
    public final MasterTender setIsAcceleratedProcedure(final Boolean newIsAcceleratedProcedure) {
        this.isAcceleratedProcedure = newIsAcceleratedProcedure;
        return this;
    }

    /**
     * Gets the accelerated procedure justification.
     *
     * @return reason for the accelerated procedure
     */
    
    public final String getAcceleratedProcedureJustification() {
        return acceleratedProcedureJustification;
    }

    /**
     * Sets the accelerated procedure justification.
     *
     * @param newAcceleratedProcedureJustification
     *         reason for the accelerated procedure
     *
     * @return this instance for chaining
     */
    public final MasterTender setAcceleratedProcedureJustification(final String newAcceleratedProcedureJustification) {
        this.acceleratedProcedureJustification = newAcceleratedProcedureJustification;
        return this;
    }

    /**
     * Gets the max bids count.
     *
     * @return the maxBidsCount
     */
    
    public final Integer getMaxBidsCount() {
        return maxBidsCount;
    }

    /**
     * Sets the max bids count.
     *
     * @param newMaxBidsCount
     *         the maxBidsCount to set
     *
     * @return this instance for chaining
     */
    public final MasterTender setMaxBidsCount(final Integer newMaxBidsCount) {
        this.maxBidsCount = newMaxBidsCount;
        return this;
    }

    /**
     * Gets the supply type.
     *
     * @return the supplyType
     */
    
    public final TenderSupplyType getSupplyType() {
        return supplyType;
    }

    /**
     * Sets the supply type.
     *
     * @param newSupplyType
     *         the supplyType to set
     *
     * @return this instance for chaining
     */
    public final MasterTender setSupplyType(final TenderSupplyType newSupplyType) {
        this.supplyType = newSupplyType;
        return this;
    }

    /**
     * Gets the size.
     *
     * @return the size
     */
    
    public final TenderSize getSize() {
        return size;
    }

    /**
     * Sets the size.
     *
     * @param newSize
     *         the size to set
     *
     * @return this instance for chaining
     */
    public final MasterTender setSize(final TenderSize newSize) {
        this.size = newSize;
        return this;
    }

    /**
     * Gets the bid deadline.
     *
     * @return the bidDeadline
     */
    
    public final LocalDateTime getBidDeadline() {
        return bidDeadline;
    }

    /**
     * Sets the bid deadline.
     *
     * @param newBidDeadline
     *         the bidDeadline to set
     *
     * @return this instance for chaining
     */
    public final MasterTender setBidDeadline(final LocalDateTime newBidDeadline) {
        this.bidDeadline = newBidDeadline;
        return this;
    }

    /**
     * Gets the documents deadline.
     *
     * @return the documentsDeadline
     */
    
    public final LocalDateTime getDocumentsDeadline() {
        return documentsDeadline;
    }

    /**
     * Sets the documents deadline.
     *
     * @param newDocumentsDeadline
     *         the documentsDeadline to set
     *
     * @return this instance for chaining
     */
    public final MasterTender setDocumentsDeadline(final LocalDateTime newDocumentsDeadline) {
        this.documentsDeadline = newDocumentsDeadline;
        return this;
    }

    /**
     * Gets the documents payable.
     *
     * @return the documentsPayable
     */
    
    public final Boolean getDocumentsPayable() {
        return documentsPayable;
    }

    /**
     * Sets the documents payable.
     *
     * @param newDocumentsPayable
     *         the documentsPayable to set
     *
     * @return this instance for chaining
     */
    public final MasterTender setDocumentsPayable(final Boolean newDocumentsPayable) {
        this.documentsPayable = newDocumentsPayable;
        return this;
    }

    /**
     * Gets the documents price.
     *
     * @return the documentsPrice
     */
    
    public final Price getDocumentsPrice() {
        return documentsPrice;
    }

    /**
     * Sets the documents price.
     *
     * @param newDocumentsPrice
     *         the documentsPrice to set
     *
     * @return this instance for chaining
     */
    public final MasterTender setDocumentsPrice(final Price newDocumentsPrice) {
        this.documentsPrice = newDocumentsPrice;
        return this;
    }

    /**
     * Gets the documents location.
     *
     * @return the documentsLocation
     */
    
    public final Address getDocumentsLocation() {
        return documentsLocation;
    }

    /**
     * Sets the documents location.
     *
     * @param newDocumentsLocation
     *         the documentsLocation to set
     *
     * @return this instance for chaining
     */
    public final MasterTender setDocumentsLocation(final Address newDocumentsLocation) {
        this.documentsLocation = newDocumentsLocation;
        return this;
    }

    /**
     * Gets the checks if is documents access restricted.
     *
     * @return true if the access to documents is free, false for restricted
     * access
     */
    
    public final Boolean getIsDocumentsAccessRestricted() {
        return isDocumentsAccessRestricted;
    }

    /**
     * Sets the is documents access restricted.
     *
     * @param newIsDocumentsAccessRestricted
     *         boolean whether the access to documents is free
     *
     * @return this instance for chaining
     */
    public final MasterTender setIsDocumentsAccessRestricted(final Boolean newIsDocumentsAccessRestricted) {
        this.isDocumentsAccessRestricted = newIsDocumentsAccessRestricted;
        return this;
    }

    /**
     * Gets the checks if is central procurement.
     *
     * @return the isCentralProcurement
     */
    
    public final Boolean getIsCentralProcurement() {
        return isCentralProcurement;
    }

    /**
     * Sets the is central procurement.
     *
     * @param newIsCentralProcurement
     *         the isCentralProcurement to set
     *
     * @return this instance for chaining
     */
    public final MasterTender setIsCentralProcurement(final Boolean newIsCentralProcurement) {
        this.isCentralProcurement = newIsCentralProcurement;
        return this;
    }

    /**
     * Gets the checks if is joint procurement.
     *
     * @return the isJointProcurement
     */
    
    public final Boolean getIsJointProcurement() {
        return isJointProcurement;
    }

    /**
     * Sets the is joint procurement.
     *
     * @param newIsJointProcurement
     *         the isJointProcurement to set
     *
     * @return this instance for chaining
     */
    public final MasterTender setIsJointProcurement(final Boolean newIsJointProcurement) {
        this.isJointProcurement = newIsJointProcurement;
        return this;
    }

    /**
     * Gets the buyers.
     *
     * @return the buyers
     */
    
    public final List<MasterBody> getBuyers() {
        return buyers;
    }

    /**
     * Sets the buyers.
     *
     * @param newBuyers
     *         the buyers to set
     *
     * @return this instance for chaining
     */
    public final MasterTender setBuyers(final List<MasterBody> newBuyers) {
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
    public final MasterTender addBuyer(final MasterBody buyer) {
        if (buyer != null) {
            if (this.buyers == null) {
                this.buyers = new ArrayList<>();
            }

            this.buyers.add(buyer);
        }

        return this;
    }

    /**
     * Gets the checks if is on behalf of.
     *
     * @return true if the purchase is being made for someone else
     */
    
    public final Boolean getIsOnBehalfOf() {
        return isOnBehalfOf;
    }

    /**
     * Sets the is on behalf of.
     *
     * @param newIsOnBehalfOf
     *         booleaN whether the purchase is being made for someone else
     *
     * @return this instance for chaining
     */
    public final MasterTender setIsOnBehalfOf(final Boolean newIsOnBehalfOf) {
        this.isOnBehalfOf = newIsOnBehalfOf;
        return this;
    }

    /**
     * Gets the on behalf of.
     *
     * @return the list of onBehalfOf
     */
    
    public final List<MasterBody> getOnBehalfOf() {
        return onBehalfOf;
    }

    /**
     * Sets the on behalf of.
     *
     * @param newOnBehalfOf
     *         the list of onBehalfOf to set
     *
     * @return this instance for chaining
     */
    public final MasterTender setOnBehalfOf(final List<MasterBody> newOnBehalfOf) {
        this.onBehalfOf = newOnBehalfOf;
        return this;
    }

    /**
     * Adds the on behalf of.
     *
     * @param newOnBehalfOf
     *         the onBehalfOf to add
     *
     * @return this instance for chaining
     */
    public final MasterTender addOnBehalfOf(final MasterBody newOnBehalfOf) {
        if (newOnBehalfOf != null) {
            if (getOnBehalfOf() == null) {
                setOnBehalfOf(new ArrayList<>());
            }

            this.onBehalfOf.add(newOnBehalfOf);
        }

        return this;
    }

    /**
     * Gets the further information provider.
     *
     * @return body from whom further information can be obtained
     */
    
    public final MasterBody getFurtherInformationProvider() {
        return furtherInformationProvider;
    }

    /**
     * Sets the further information provider.
     *
     * @param newFurtherInformationProvider
     *         body from whom further information can be obtained *
     *
     * @return this instance for chaining
     */
    public final MasterTender setFurtherInformationProvider(final MasterBody newFurtherInformationProvider) {
        this.furtherInformationProvider = newFurtherInformationProvider;
        return this;
    }

    /**
     * Gets the specifications provider.
     *
     * @return body from whom specifications and additional documents can be
     * obtained
     */
    
    public final MasterBody getSpecificationsProvider() {
        return specificationsProvider;
    }

    /**
     * Sets the specifications provider.
     *
     * @param newSpecificationsProvider
     *         body from whom specifications and additional documents can be
     *         obtained
     *
     * @return this instance for chaining
     */
    public final MasterTender setSpecificationsProvider(final MasterBody newSpecificationsProvider) {
        this.specificationsProvider = newSpecificationsProvider;
        return this;
    }

    /**
     * Gets the bids recipient.
     *
     * @return body to whom tenders/requests to participate must be sent
     */
    
    public final MasterBody getBidsRecipient() {
        return bidsRecipient;
    }

    /**
     * Sets the bids recipient.
     *
     * @param newBidsRecipient
     *         body to whom tenders/requests to participate must be sent
     *
     * @return this instance for chaining
     */
    public final MasterTender setBidsRecipient(final MasterBody newBidsRecipient) {
        this.bidsRecipient = newBidsRecipient;
        return this;
    }

    /**
     * Gets the publications.
     *
     * @return the public finalations
     */
    
    public final List<Publication> getPublications() {
        return publications;
    }

    /**
     * Sets the publications.
     *
     * @param newPublications
     *         the publications to set
     *
     * @return this instance for chaining
     */
    public final MasterTender setPublications(final List<Publication> newPublications) {
        this.publications = newPublications;
        return this;
    }

    /**
     * Gets the administrators.
     *
     * @return the administrators
     */
    
    public final List<MasterBody> getAdministrators() {
        return administrators;
    }

    /**
     * Sets the administrators.
     *
     * @param newAdministrators
     *         the administrators to set
     *
     * @return this instance for chaining
     */
    public final MasterTender setAdministrators(final List<MasterBody> newAdministrators) {
        this.administrators = newAdministrators;
        return this;
    }

    /**
     * Gets the supervisors.
     *
     * @return the supervisors
     */
    
    public final List<MasterBody> getSupervisors() {
        return supervisors;
    }

    /**
     * Sets the supervisors.
     *
     * @param newSupervisors
     *         the supervisors to set
     *
     * @return this instance for chaining
     */
    public final MasterTender setSupervisors(final List<MasterBody> newSupervisors) {
        this.supervisors = newSupervisors;
        return this;
    }

    /**
     * Gets the specifications creator.
     *
     * @return the specificationsCreator
     */
    
    public final MasterBody getSpecificationsCreator() {
        return specificationsCreator;
    }

    /**
     * Sets the specifications creator.
     *
     * @param newSpecificationsCreator
     *         the specificationsCreator to set
     *
     * @return this instance for chaining
     */
    public final MasterTender setSpecificationsCreator(final MasterBody newSpecificationsCreator) {
        this.specificationsCreator = newSpecificationsCreator;
        return this;
    }

    /**
     * Gets the checks for lots.
     *
     * @return is tender divided into lots
     */
    
    public final Boolean getHasLots() {
        return hasLots;
    }

    /**
     * Sets the has lots.
     *
     * @param newHasLots
     *         is tender divided into lots
     *
     * @return this instance for chaining
     */
    public final MasterTender setHasLots(final Boolean newHasLots) {
        this.hasLots = newHasLots;
        return this;
    }

    /**
     * Gets the lots.
     *
     * @return the lots
     */
    
    public final List<MasterTenderLot> getLots() {
        return lots;
    }

    /**
     * Sets the lots.
     *
     * @param newLots
     *         the lots to set
     *
     * @return this instance for chaining
     */
    public final MasterTender setLots(final List<? extends MasterTenderLot> newLots) {
        this.lots = (List<MasterTenderLot>) newLots;
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
    public final MasterTender addLot(final MasterTenderLot lot) {
        if (lot != null) {
            if (this.lots == null) {
                setLots(new ArrayList<>());
            }

            this.lots.add(lot);
        }

        return this;
    }

    /**
     * Gets the candidates.
     *
     * @return the candidates
     */
    
    public final List<MasterBody> getCandidates() {
        return candidates;
    }

    /**
     * Sets the candidates.
     *
     * @param newCandidates
     *         the candidates to set
     *
     * @return this instance for chaining
     */
    public final MasterTender setCandidates(final List<MasterBody> newCandidates) {
        this.candidates = newCandidates;
        return this;
    }

    /**
     * Gets the approached bidders.
     *
     * @return the approachedBidders
     */
    
    public final List<MasterBody> getApproachedBidders() {
        return approachedBidders;
    }

    /**
     * Sets the approached bidders.
     *
     * @param newApproachedBidders
     *         the approachedBidders to set
     *
     * @return this instance for chaining
     */
    public final MasterTender setApproachedBidders(final List<MasterBody> newApproachedBidders) {
        this.approachedBidders = newApproachedBidders;
        return this;
    }

    /**
     * Gets the documents.
     *
     * @return the documents
     */
    
    public final List<Document> getDocuments() {
        return documents;
    }

    /**
     * Sets the documents.
     *
     * @param newDocuments
     *         the documents to set
     *
     * @return this instance for chaining
     */
    public final MasterTender setDocuments(final List<Document> newDocuments) {
        this.documents = newDocuments;
        return this;
    }

    /**
     * Gets the court proceedings.
     *
     * @return the courtProceedings
     */
    
    public final List<URL> getCourtProceedings() {
        return courtProceedings;
    }

    /**
     * Sets the court proceedings.
     *
     * @param newCourtProceedings
     *         the courtProceedings to set
     *
     * @return this instance for chaining
     */
    public final MasterTender setCourtProceedings(final List<URL> newCourtProceedings) {
        this.courtProceedings = newCourtProceedings;
        return this;
    }

    /**
     * Gets the court interventions.
     *
     * @return the courtInterventions
     */
    
    public final List<URL> getCourtInterventions() {
        return courtInterventions;
    }

    /**
     * Sets the court interventions.
     *
     * @param newCourtInterventions
     *         the courtInterventions to set
     *
     * @return this instance for chaining
     */
    public final MasterTender setCourtInterventions(final List<URL> newCourtInterventions) {
        this.courtInterventions = newCourtInterventions;
        return this;
    }

    /**
     * Gets the npwp reasons.
     *
     * @return the npwpReasons
     */
    
    public final List<NpwpReason> getNpwpReasons() {
        return npwpReasons;
    }

    /**
     * Sets the npwp reasons.
     *
     * @param newNpwpReasons
     *         the npwpReasons to set
     *
     * @return this instance for chaining
     */
    public final MasterTender setNpwpReasons(final List<NpwpReason> newNpwpReasons) {
        this.npwpReasons = newNpwpReasons;
        return this;
    }

    /**
     * Gets the deposits.
     *
     * @return the deposits
     */
    
    public final String getDeposits() {
        return deposits;
    }

    /**
     * Sets the deposits.
     *
     * @param newDeposits
     *         the deposits to set
     *
     * @return this instance for chaining
     */
    public final MasterTender setDeposits(final String newDeposits) {
        this.deposits = newDeposits;
        return this;
    }

    /**
     * Gets the personal requirements.
     *
     * @return the personalRequirements
     */
    
    public final String getPersonalRequirements() {
        return personalRequirements;
    }

    /**
     * Sets the personal requirements.
     *
     * @param newPersonalRequirements
     *         the personalRequirements to set
     *
     * @return this instance for chaining
     */
    public final MasterTender setPersonalRequirements(final String newPersonalRequirements) {
        this.personalRequirements = newPersonalRequirements;
        return this;
    }

    /**
     * Gets the economic requirements.
     *
     * @return the economicRequirements
     */
    
    public final String getEconomicRequirements() {
        return economicRequirements;
    }

    /**
     * Sets the economic requirements.
     *
     * @param newEconomicRequirements
     *         the economicRequirements to set
     *
     * @return this instance for chaining
     */
    public final MasterTender setEconomicRequirements(final String newEconomicRequirements) {
        this.economicRequirements = newEconomicRequirements;
        return this;
    }

    /**
     * Gets the technical requirements.
     *
     * @return the technicalRequirements
     */
    
    public final String getTechnicalRequirements() {
        return technicalRequirements;
    }

    /**
     * Sets the technical requirements.
     *
     * @param newTechnicalRequirements
     *         the technicalRequirements to set
     *
     * @return this instance for chaining
     */
    public final MasterTender setTechnicalRequirements(final String newTechnicalRequirements) {
        this.technicalRequirements = newTechnicalRequirements;
        return this;
    }

    /**
     * Gets the appeal body name.
     *
     * @return the appealBodyName
     */
    
    public final String getAppealBodyName() {
        return appealBodyName;
    }

    /**
     * Sets the appeal body name.
     *
     * @param newAppealBodyName
     *         the appealBodyName to set
     *
     * @return this instance for chaining
     */
    public final MasterTender setAppealBodyName(final String newAppealBodyName) {
        this.appealBodyName = newAppealBodyName;
        return this;
    }

    /**
     * Gets the mediation body name.
     *
     * @return the mediationBodyName
     */
    
    public final String getMediationBodyName() {
        return mediationBodyName;
    }

    /**
     * Sets the mediation body name.
     *
     * @param newMediationBodyName
     *         the mediationBodyName to set
     *
     * @return this instance for chaining
     */
    public final MasterTender setMediationBodyName(final String newMediationBodyName) {
        this.mediationBodyName = newMediationBodyName;
        return this;
    }

    /**
     * Gets the excessive framework agreement justification.
     *
     * @return justification for framework agreement going over 4 years
     */
    
    public final String getExcessiveFrameworkAgreementJustification() {
        return excessiveFrameworkAgreementJustification;
    }

    /**
     * Sets the excessive framework agreement justification.
     *
     * @param newExcessiveFrameworkAgreementJustification
     *         justification for framework agreement going over 4 years
     *
     * @return this instance for chaining
     */
    public final MasterTender setExcessiveFrameworkAgreementJustification(
            final String newExcessiveFrameworkAgreementJustification) {
        this.excessiveFrameworkAgreementJustification = newExcessiveFrameworkAgreementJustification;
        return this;
    }

    /**
     * Gets the checks if is whole tender cancelled.
     *
     * @return true if the whole tender is cancelled, false if just some of its
     * lots
     */
    
    public final Boolean getIsWholeTenderCancelled() {
        return isWholeTenderCancelled;
    }

    /**
     * Sets the is whole tender cancelled.
     *
     * @param newWholeTenderCancelled
     *         whether the whole tender is cancelled (or just some of its
     *         lots)
     *
     * @return this instance for chaining
     */
    public final MasterTender setIsWholeTenderCancelled(final Boolean newWholeTenderCancelled) {
        this.isWholeTenderCancelled = newWholeTenderCancelled;
        return this;
    }

    /**
     * Gets the enquiry deadline.
     *
     * @return the enquiryDeadline
     */
    
    public final LocalDate getEnquiryDeadline() {
        return enquiryDeadline;
    }

    /**
     * Sets the enquiry deadline.
     *
     * @param newEnquiryDeadline
     *         the enquiryDeadline to set
     *
     * @return this instance for chaining
     */
    public final MasterTender setEnquiryDeadline(final LocalDate newEnquiryDeadline) {
        this.enquiryDeadline = newEnquiryDeadline;
        return this;
    }

    /**
     * Gets the award deadline.
     *
     * @return the awardDeadline
     */
    
    public final LocalDate getAwardDeadline() {
        return awardDeadline;
    }

    /**
     * Sets the award deadline.
     *
     * @param newAwardDeadline
     *         the awardDeadline to set
     *
     * @return this instance for chaining
     */
    public final MasterTender setAwardDeadline(final LocalDate newAwardDeadline) {
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
    public final MasterTender setAwardDeadlineDuration(final Integer newAwardDeadlineDuration) {
        this.awardDeadlineDuration = newAwardDeadlineDuration;
        return this;
    }

    /**
     * Gets the final price.
     *
     * @return the finalPrice
     */
    
    public final Price getFinalPrice() {
        return finalPrice;
    }

    /**
     * Sets the final price.
     *
     * @param newFinalPrice
     *         the finalPrice to set
     *
     * @return this instance for chaining
     */
    public final MasterTender setFinalPrice(final Price newFinalPrice) {
        this.finalPrice = newFinalPrice;
        return this;
    }

    /**
     * Gets the eligible bid languages.
     *
     * @return the eligibleBidLanguages
     */
    
    public final List<String> getEligibleBidLanguages() {
        return eligibleBidLanguages;
    }

    /**
     * Sets the eligible bid languages.
     *
     * @param newEligibleBidLanguages
     *         the eligibleBidLanguages to set
     *
     * @return this instance for chaining
     */
    public final MasterTender setEligibleBidLanguages(final List<String> newEligibleBidLanguages) {
        this.eligibleBidLanguages = newEligibleBidLanguages;
        return this;
    }

    /**
     * Gets the checks if is E invoice accepted.
     *
     * @return the isEInvoiceAccepted
     */
    
    public final Boolean getIsEInvoiceAccepted() {
        return isEInvoiceAccepted;
    }

    /**
     * Sets the is E invoice accepted.
     *
     * @param newIsEInvoiceAccepted
     *         the isEInvoiceAccepted to set
     *
     * @return this instance for chaining
     */
    public final MasterTender setIsEInvoiceAccepted(final Boolean newIsEInvoiceAccepted) {
        this.isEInvoiceAccepted = newIsEInvoiceAccepted;
        return this;
    }

    /**
     * Gets the corrections.
     *
     * @return the corrections
     */
    
    public final List<Corrigendum> getCorrections() {
        return corrections;
    }

    /**
     * Sets the corrections.
     *
     * @param newCorrections
     *         the corrections to set
     *
     * @return this instance for chaining
     */
    public final MasterTender setCorrections(final List<Corrigendum> newCorrections) {
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
    public final MasterTender setModificationReason(final String modificationReason) {
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
    public final MasterTender setModificationReasonDescription(final String modificationReasonDescription) {
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
    public final MasterTender setAdditionalInfo(final String additionalInfo) {
        this.additionalInfo = additionalInfo;
        return this;
    }

    /**
     * Gets the group id.
     *
     * @return the group id
     */
    public final String getGroupId() {
        return groupId;
    }

    /**
     * Sets the group id.
     *
     * @param newGroupId
     *         the new group id
     *
     * @return the master tender
     */
    public final MasterTender setGroupId(final String newGroupId) {
        this.groupId = newGroupId;
        return this;
    }

    /**
     * Gets the master by.
     *
     * @return the master by
     */
    public final String getMasterBy() {
        return masterBy;
    }

    /**
     * Sets the master by.
     *
     * @param newMasterBy
     *         the new master by
     *
     * @return the master tender
     */
    public final MasterTender setMasterBy(final String newMasterBy) {
        this.masterBy = newMasterBy;
        return this;
    }

    /**
     * Gets the hash.
     *
     * @return the hash
     */
    public final String getHash() {
        return hash;
    }

    /**
     * Sets the hash.
     *
     * @param newHash
     *         the new hash
     *
     * @return the master tender
     */
    public final MasterTender setHash(final String newHash) {
        this.hash = newHash;
        return this;
    }
    
    /**
     * @return the indicators
     */
    public final List<Indicator> getIndicators() {
        return indicators;
    }

    /**
     * @param indicators the indicators to set
     * 
     * @return the master tender
     */
    public final MasterTender setIndicators(final List<Indicator> indicators) {
        this.indicators = indicators;
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
    public final MasterTender setCountry(final String country) {
        this.country = country;
        return this;
    }

    /**
     * @return digiwhist price
     */
    public final Price getDigiwhistPrice() {
        return digiwhistPrice;
    }

    /**
     * @param digiwhistPrice
     *      digiwhist price to be set
     * @return this instance for chaining
     */
    public final MasterTender setDigiwhistPrice(final Price digiwhistPrice) {
        this.digiwhistPrice = digiwhistPrice;
        return this;
    }
}
