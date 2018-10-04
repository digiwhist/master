package eu.dl.dataaccess.dto.clean;

import static eu.dl.dataaccess.utils.ClassUtils.removeNonsenses;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;


import com.fasterxml.jackson.annotation.JsonIgnore;

import eu.dl.dataaccess.annotation.Transformable;
import eu.dl.dataaccess.dto.codetables.TenderLotStatus;
import eu.dl.dataaccess.dto.generic.Price;
import eu.dl.dataaccess.utils.ValidationUtils;

/**
 * Tender lot. Info on individual lot of a contract. It is preferred to have
 * data on lot level, rather than on contract level, as this is more precise.
 * For most contracts there is just one lot.
 */
@Transformable
public final class CleanTenderLot extends BaseCleanTenderLot<CleanTenderLot> implements Validable {
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
    private List<CleanBid> bids;

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
    public CleanTenderLot setContractNumber(final String contractNumber) {
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
    public CleanTenderLot setLotNumber(final Integer lotNumber) {
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
    public CleanTenderLot setPositionOnPage(final Integer positionOnPage) {
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
    public CleanTenderLot setStatus(final TenderLotStatus status) {
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
     * @return the clean tender lot
     */
    public CleanTenderLot setRobustEstimatedPrice(final Price newRobustEstimatedPrice) {
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
    public CleanTenderLot setCompletionDate(final LocalDate completionDate) {
        this.completionDate = completionDate;
        return this;
    }

    /**
     * Gets the bids.
     *
     * @return the bids
     */
    public List<CleanBid> getBids() {
        return bids;
    }

    /**
     * Sets the bids.
     *
     * @param bids
     *            the bids to set
     * @return this instance for chaining
     */
    public CleanTenderLot setBids(final List<CleanBid> bids) {
        this.bids = bids;
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
    public CleanTenderLot setBidsCount(final Integer bidsCount) {
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
    public CleanTenderLot setValidBidsCount(final Integer validBidsCount) {
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
    public CleanTenderLot setElectronicBidsCount(final Integer electronicBidsCount) {
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
    public CleanTenderLot setSmeBidsCount(final Integer smeBidsCount) {
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
    public CleanTenderLot setOtherEuMemberStatesCompaniesBidsCount(
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
    public CleanTenderLot setNonEuMemberStatesCompaniesBidsCount(
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
    public CleanTenderLot setForeignCompaniesBidsCount(final Integer foreignCompaniesBidsCount) {
        this.foreignCompaniesBidsCount = foreignCompaniesBidsCount;
        return this;
    }

    /**
     * Adds bids to the list of bids or creates a new list with given bids if none
     * exists.
     *
     * @param newBids
     *            new bids to be added
     *
     * @return this instance for chaining
     */
    public CleanTenderLot addBids(final List<CleanBid> newBids) {
        if (newBids != null) {
            if (getBids() == null) {
                setBids(new ArrayList<>());
            }
            this.bids.addAll(newBids);
        }

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
     * @return the clean tender lot
     */
    public CleanTenderLot setLotId(final String lotId) {
        this.lotId = lotId;
        return this;
    }

    @Override
    
    @JsonIgnore
    public CleanTenderLot getValid() {
        setAddressOfImplementation(removeNonsenses(addressOfImplementation));
        setAwardCriteria(ValidationUtils.getValid(awardCriteria));
        setBids(ValidationUtils.getValid(bids));
        setCpvs(ValidationUtils.getValid(cpvs));
        setEstimatedPrice(removeNonsenses(estimatedPrice));
        setFundings(ValidationUtils.getValid(fundings));
        setRobustEstimatedPrice(removeNonsenses(robustEstimatedPrice));

        // for CleanTenderLot is not necessary to check whether is empty, we assume that each lot has at least one
        // not null parameter.
        return this;
    }
}
