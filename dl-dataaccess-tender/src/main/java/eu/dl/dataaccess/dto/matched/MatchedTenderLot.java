package eu.dl.dataaccess.dto.matched;

import com.fasterxml.jackson.annotation.JsonIgnore;
import eu.dl.dataaccess.dto.clean.CleanTenderLot;
import eu.dl.dataaccess.dto.codetables.TenderLotStatus;
import eu.dl.dataaccess.dto.generic.Price;
import eu.dl.dataaccess.dto.utils.InitUtils;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Tender lot. Info on individual lot of a contract. It is preferred to have
 * data on lot level, rather than on contract level, as this is more precise.
 * For most contracts there is just one lot.
 */
public final class MatchedTenderLot extends BaseMatchedTenderLot<MatchedTenderLot> implements MasterablePart {
    /**
     * Contract(the document, not tender!) number.
     */
    private String contractNumber;

    /**
     * Lot number.
     */
    private Integer lotNumber;

    /**
     * Order on page.
     */
    private Integer positionOnPage;

    /**
     * Tender lot status (prepared, awaiting bids, evaluating bids, awarded,
     * finished, cancelled).
     */
    private TenderLotStatus status;

    /**
     * Calculated estimated price (the best guessed estimated price), calculated according to the algorithm described on
     * zindex wiki (http://wiki.zindex.cz/doku.php?id=internal:urceni_ceny).
     */
    private Price robustEstimatedPrice;

    /**
     * Real completion date (signature of protocol, date of delivery...).
     */
    private LocalDate completionDate;

    /**
     * List of received bids.
     */
    private List<MatchedBid> bids;

    /**
     * Number of bids received including electronic bids.
     */
    private Integer bidsCount;

    /**
     * Number of bids considered.
     */
    private Integer validBidsCount;

    /**
     * Number of bids received via electronic means.
     */
    private Integer electronicBidsCount;

    /**
     * Number of bids received from SMEs.
     */
    private Integer smeBidsCount;

    /**
     * Number of bids received from countries from other EU Member States.
     */
    private Integer otherEuMemberStatesCompaniesBidsCount;

    /**
     * Number of bids received from countries from non-EU Member States.
     */
    private Integer nonEuMemberStatesCompaniesBidsCount;

    /**
     * Number of foreign participants.
     */
    private Integer foreignCompaniesBidsCount;

    /**
     * Lot id. This one must be unique within superior tender object.
     */
    private String lotId;

    /**
     * Structured Lot id.
     */
    private StructuredLotId structuredId;

    /**
     * Publication date.
     */
    private LocalDate publicationDate;

    /**
     * Creates empty matched tender lot.
     */
    public MatchedTenderLot() {
        super();
    }

    /**
     * Initialization of tender lot from clean lot item.
     * 
     * @param cleanLot
     *            clean lot item
     */
    public MatchedTenderLot(final CleanTenderLot cleanLot) {
        super();
        setContractNumber(cleanLot.getContractNumber());
        setLotNumber(cleanLot.getLotNumber());
        setPositionOnPage(cleanLot.getPositionOnPage());
        setStatus(cleanLot.getStatus());
        setRobustEstimatedPrice(cleanLot.getRobustEstimatedPrice());
        setCompletionDate(cleanLot.getCompletionDate());
        setBids(InitUtils.cleanToMatchedBid(cleanLot.getBids()));
        setBidsCount(cleanLot.getBidsCount());
        setValidBidsCount(cleanLot.getValidBidsCount());
        setElectronicBidsCount(cleanLot.getElectronicBidsCount());
        setSmeBidsCount(cleanLot.getSmeBidsCount());
        setOtherEuMemberStatesCompaniesBidsCount(cleanLot.getOtherEuMemberStatesCompaniesBidsCount());
        setNonEuMemberStatesCompaniesBidsCount(cleanLot.getNonEuMemberStatesCompaniesBidsCount());
        setForeignCompaniesBidsCount(cleanLot.getForeignCompaniesBidsCount());
        setLotId(cleanLot.getLotId());

        // shared in a lot and a tender
        setTitle(cleanLot.getTitle());
        setTitleEnglish(cleanLot.getTitleEnglish());
        setDescription(cleanLot.getDescription());
        setDescriptionEnglish(cleanLot.getDescriptionEnglish());
        setIsAwarded(cleanLot.getIsAwarded());
        setAreVariantsAccepted(cleanLot.getAreVariantsAccepted());
        setHasOptions(cleanLot.getHasOptions());
        setEligibilityCriteria(cleanLot.getEligibilityCriteria());
        setIsCoveredByGpa(cleanLot.getIsCoveredByGpa());
        setIsFrameworkAgreement(cleanLot.getIsFrameworkAgreement());
        setMaxFrameworkAgreementParticipants(cleanLot.getMaxFrameworkAgreementParticipants());
        setAddressOfImplementation(cleanLot.getAddressOfImplementation());
        setIsDps(cleanLot.getIsDps());
        setEstimatedStartDate(cleanLot.getEstimatedStartDate());
        setEstimatedCompletionDate(cleanLot.getEstimatedCompletionDate());
        setEstimatedDurationInYears(cleanLot.getEstimatedDurationInYears());
        setEstimatedDurationInMonths(cleanLot.getEstimatedDurationInMonths());
        setEstimatedDurationInDays(cleanLot.getEstimatedDurationInDays());
        setAwardDecisionDate(cleanLot.getAwardDecisionDate());
        setContractSignatureDate(cleanLot.getContractSignatureDate());
        setCpvs(cleanLot.getCpvs());
        setFundings(cleanLot.getFundings());
        setSelectionMethod(cleanLot.getSelectionMethod());
        setAwardCriteria(cleanLot.getAwardCriteria());
        setIsElectronicAuction(cleanLot.getIsElectronicAuction());
        setCancellationDate(cleanLot.getCancellationDate());
        setCancellationReason(cleanLot.getCancellationReason());
        setEstimatedPrice(cleanLot.getEstimatedPrice());
        setEnvisagedCandidatesCount(cleanLot.getEnvisagedCandidatesCount());
        setEnvisagedMinCandidatesCount(cleanLot.getEnvisagedMinCandidatesCount());
        setEnvisagedMaxCandidatesCount(cleanLot.getEnvisagedMaxCandidatesCount());
        setLimitedCandidatesCountCriteria(cleanLot.getLimitedCandidatesCountCriteria());
    }

    /**
     * Gets the contract number.
     *
     * @return the contractNumber
     */
    public String getContractNumber() {
        return contractNumber;
    }

    /**
     * Sets the contract number.
     *
     * @param contractNumber
     *            the contractNumber to set
     * @return this instance for chaining
     */
    public MatchedTenderLot setContractNumber(final String contractNumber) {
        this.contractNumber = contractNumber;
        return this;
    }

    /**
     * Gets the lot number.
     *
     * @return the lotNumber
     */
    public Integer getLotNumber() {
        return lotNumber;
    }

    /**
     * Sets the lot number.
     *
     * @param lotNumber
     *            the lotNumber to set
     * @return this instance for chaining
     */
    public MatchedTenderLot setLotNumber(final Integer lotNumber) {
        this.lotNumber = lotNumber;
        return this;
    }

    /**
     * Gets the position on page.
     *
     * @return the positionOnPage
     */
    public Integer getPositionOnPage() {
        return positionOnPage;
    }

    /**
     * Sets the position on page.
     *
     * @param positionOnPage
     *            the positionOnPage to set
     * @return this instance for chaining
     */
    public MatchedTenderLot setPositionOnPage(final Integer positionOnPage) {
        this.positionOnPage = positionOnPage;
        return this;
    }

    /**
     * Gets the status.
     *
     * @return the status
     */
    public TenderLotStatus getStatus() {
        return status;
    }

    /**
     * Sets the status.
     *
     * @param status
     *            the status to set
     * @return this instance for chaining
     */
    public MatchedTenderLot setStatus(final TenderLotStatus status) {
        this.status = status;
        return this;
    }

    /**
     * Gets the robust estimated price.
     *
     * @return the robust estimated price
     */
    public Price getRobustEstimatedPrice() {
        return robustEstimatedPrice;
    }

    /**
     * Sets the robust estimated price.
     *
     * @param newRobustEstimatedPrice
     *            the new robust estimated price
     * @return the matched tender lot
     */
    public MatchedTenderLot setRobustEstimatedPrice(final Price newRobustEstimatedPrice) {
        this.robustEstimatedPrice = newRobustEstimatedPrice;
        return this;
    }

    /**
     * Gets the completion date.
     *
     * @return the completionDate
     */
    public LocalDate getCompletionDate() {
        return completionDate;
    }

    /**
     * Sets the completion date.
     *
     * @param completionDate
     *            the completionDate to set
     * @return this instance for chaining
     */
    public MatchedTenderLot setCompletionDate(final LocalDate completionDate) {
        this.completionDate = completionDate;
        return this;
    }

    /**
     * Gets the bids.
     *
     * @return the bids
     */
    public List<MatchedBid> getBids() {
        return bids;
    }

    /**
     * Sets the bids.
     *
     * @param bids
     *            the bids to set
     * @return this instance for chaining
     */
    public MatchedTenderLot setBids(final List<MatchedBid> bids) {
        this.bids = bids;
        return this;
    }

    /**
     * Adds bid to the list of bids or create a new list with given bid if none exists.
     *
     * @param bid
     *            new bid to be added
     *
     * @return this instance for chaining
     */
    public MatchedTenderLot addBid(final MatchedBid bid) {
        if (getBids() == null) {
            setBids(new ArrayList<>());
        }
        this.bids.add(bid);
        return this;
    }

    /**
     * Gets the bids count.
     *
     * @return the bidsCount
     */
    public Integer getBidsCount() {
        return bidsCount;
    }

    /**
     * Sets the bids count.
     *
     * @param bidsCount
     *            the bidsCount to set
     * @return this instance for chaining
     */
    public MatchedTenderLot setBidsCount(final Integer bidsCount) {
        this.bidsCount = bidsCount;
        return this;
    }

    /**
     * Gets the valid bids count.
     *
     * @return the validBidsCount
     */
    public Integer getValidBidsCount() {
        return validBidsCount;
    }

    /**
     * Sets the valid bids count.
     *
     * @param validBidsCount
     *            the validBidsCount to set
     * @return this instance for chaining
     */
    public MatchedTenderLot setValidBidsCount(final Integer validBidsCount) {
        this.validBidsCount = validBidsCount;
        return this;
    }

    /**
     * Gets the electronic bids count.
     *
     * @return the electronicBidsCount
     */
    public Integer getElectronicBidsCount() {
        return electronicBidsCount;
    }

    /**
     * Sets the electronic bids count.
     *
     * @param electronicBidsCount
     *            the electronicBidsCount to set
     * @return this instance for chaining
     */
    public MatchedTenderLot setElectronicBidsCount(final Integer electronicBidsCount) {
        this.electronicBidsCount = electronicBidsCount;
        return this;
    }

    /**
     * Gets the sme bids count.
     *
     * @return the smeBidsCount
     */
    public Integer getSmeBidsCount() {
        return smeBidsCount;
    }

    /**
     * Sets the sme bids count.
     *
     * @param smeBidsCount
     *            the smeBidsCount to set
     * @return this instance for chaining
     */
    public MatchedTenderLot setSmeBidsCount(final Integer smeBidsCount) {
        this.smeBidsCount = smeBidsCount;
        return this;
    }

    /**
     * Gets the other eu member states companies bids count.
     *
     * @return the otherEuMemberStatesCompaniesBidsCount
     */
    public Integer getOtherEuMemberStatesCompaniesBidsCount() {
        return otherEuMemberStatesCompaniesBidsCount;
    }

    /**
     * Sets the other eu member states companies bids count.
     *
     * @param otherEuMemberStatesCompaniesBidsCount
     *            the otherEuMemberStatesCompaniesBidsCount to set
     * @return this instance for chaining
     */
    public MatchedTenderLot setOtherEuMemberStatesCompaniesBidsCount(
            final Integer otherEuMemberStatesCompaniesBidsCount) {
        this.otherEuMemberStatesCompaniesBidsCount = otherEuMemberStatesCompaniesBidsCount;
        return this;
    }

    /**
     * Gets the non eu member states companies bids count.
     *
     * @return the nonEuMemberStatesCompaniesBidsCount
     */
    public Integer getNonEuMemberStatesCompaniesBidsCount() {
        return nonEuMemberStatesCompaniesBidsCount;
    }

    /**
     * Sets the non eu member states companies bids count.
     *
     * @param nonEuMemberStatesCompaniesBidsCount
     *            the nonEuMemberStatesCompaniesBidsCount to set
     * @return this instance for chaining
     */
    public MatchedTenderLot setNonEuMemberStatesCompaniesBidsCount(
            final Integer nonEuMemberStatesCompaniesBidsCount) {
        this.nonEuMemberStatesCompaniesBidsCount = nonEuMemberStatesCompaniesBidsCount;
        return this;
    }

    /**
     * Gets the foreign companies bids count.
     *
     * @return number of foreign participants
     */
    public Integer getForeignCompaniesBidsCount() {
        return foreignCompaniesBidsCount;
    }

    /**
     * Sets the foreign companies bids count.
     *
     * @param foreignCompaniesBidsCount
     *            number of foreign participants
     * @return this instance for chaining
     */
    public MatchedTenderLot setForeignCompaniesBidsCount(final Integer foreignCompaniesBidsCount) {
        this.foreignCompaniesBidsCount = foreignCompaniesBidsCount;
        return this;
    }
    
    /**
     * Gets the lot id.
     *
     * @return the lot id
     */
    public String getLotId() {
        return lotId;
    }

    /**
     * Sets the lot id.
     *
     * @param lotId
     *            the lot id
     * @return the matched tender lot
     */
    public MatchedTenderLot setLotId(final String lotId) {
        this.lotId = lotId;
        return this;
    }

    /**
     * Gets structuredId.
     *
     * @return value of structuredId
     */
    public StructuredLotId getStructuredId() {
        return structuredId;
    }

    /**
     * Sets structuredId.
     *
     * @param structuredId
     *         the structuredId to set
     *
     * @return this instance for chaining
     */
    public MatchedTenderLot setStructuredId(final StructuredLotId structuredId) {
        this.structuredId = structuredId;
        return this;
    }

    /**
     * Gets bids and sets them structured id.
     *
     * @return bids with set structured id
     */
    @JsonIgnore
    public List<MatchedBid> getBidsWithStructuredId() {
        if (getBids() == null || getStructuredId() == null) {
            return null;
        }
        return getBids().stream()
                .map(b -> b.setStructuredId(new StructuredBidId()
                        .setTenderId(getStructuredId().getTenderId())
                        .setLotId(getLotId())
                        .setBidId(b.getBidId())))
                .collect(Collectors.toList());
    }

    @Override
    public String getTenderId() {
        if (getStructuredId() == null) {
            return null;
        }

        return getStructuredId().getTenderId();
    }

    @Override
    public LocalDate getPublicationDate() {
        return publicationDate;
    }

    /**
     * @param newPublicationDate
     *            the lot publication date to set
     * @return this instance for chaining
     */
    public MatchedTenderLot setPublicationDate(final LocalDate newPublicationDate) {
        this.publicationDate = newPublicationDate;
        return this;
    }
}
