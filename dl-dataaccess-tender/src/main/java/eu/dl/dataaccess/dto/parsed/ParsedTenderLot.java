package eu.dl.dataaccess.dto.parsed;

import java.util.ArrayList;
import java.util.List;

/**
 * Tender lot. Info on individual lot of a contract. It is preferred to have
 * data on lot level, rater than on contract level, as this is more precise. For
 * most contracts there is just one lot.
 */
public final class ParsedTenderLot extends BaseParsedTenderLot<ParsedTenderLot> {
    /**
     * Contract(the document, not tender!) number.
     */
    private String contractNumber;

    /**
     * Lot number.
     */
    private String lotNumber;

    /**
     * Order on page.
     */
    private String positionOnPage;

    /**
     * Tender part status (prepared, awaiting bids, evaluating bids, awarded,
     * finished, cancelled).
     */
    private String status;

    /**
     * Calculated estimated price (the best guessed estimated price), calculated
     * according to the algorithm described on zindex wiki
     * (http://wiki.zindex.cz/doku.php?id=internal:urceni_ceny).
     */
    private ParsedPrice robustEstimatedPrice;

    /**
     * Real completion date (signature of protocol, date of delivery...).
     */
    private String completionDate;

    /**
     * List of received bids.
     */
    private List<ParsedBid> bids;

    /**
     * Number of bids received including electronic bids.
     */
    private String bidsCount;

    /**
     * Number of bids considered.
     */
    private String validBidsCount;

    /**
     * Number of bids received via electronic means.
     */
    private String electronicBidsCount;

    /**
     * Number of bids received from SMEs.
     */
    private String smeBidsCount;

    /**
     * Number of bids received from countries from other EU Member States.
     */
    private String otherEuMemberStatesCompaniesBidsCount;

    /**
     * Number of bids received from countries from non-EU Member States.
     */
    private String nonEuMemberStatesCompaniesBidsCount;

    /**
     * Number of foreign participants.
     */
    private String foreignCompaniesBidsCount;

    /**
     * Lot id. This one must be unique within superior tender object.
     */
    private String lotId;

    /**
     * @return the contractNumber
     */

    public String getContractNumber() {
        return contractNumber;
    }

    /**
     * @param contractNumber
     *            the contractNumber to set
     *
     * @return this instance for for chaining
     */

    public ParsedTenderLot setContractNumber(final String contractNumber) {
        this.contractNumber = contractNumber;
        return this;
    }

    /**
     * @return the lotNumber
     */

    public String getLotNumber() {
        return lotNumber;
    }

    /**
     * @param lotNumber
     *            the lotNumber to set
     *
     * @return this instance for for chaining
     */

    public ParsedTenderLot setLotNumber(final String lotNumber) {
        this.lotNumber = lotNumber;
        return this;
    }

    /**
     * @return the positionOnPage
     */

    public String getPositionOnPage() {
        return positionOnPage;
    }

    /**
     * @param positionOnPage
     *            the positionOnPage to set
     *
     * @return this instance for for chaining
     */

    public ParsedTenderLot setPositionOnPage(final String positionOnPage) {
        this.positionOnPage = positionOnPage;
        return this;
    }

    /**
     * @return the status
     */

    public String getStatus() {
        return status;
    }

    /**
     * @param status
     *            the status to set
     *
     * @return this instance for for chaining
     */

    public ParsedTenderLot setStatus(final String status) {
        this.status = status;
        return this;
    }

    /**
     * Robust parsed price setter.
     * 
     * @return self
     */
    public ParsedPrice getRobustEstimatedPrice() {
        return robustEstimatedPrice;
    }

    /**
     * Getter.
     * 
     * @param robustEstimatedPrice
     *            value
     * @return self
     */
    public ParsedTenderLot setRobustEstimatedPrice(final ParsedPrice robustEstimatedPrice) {
        this.robustEstimatedPrice = robustEstimatedPrice;
        return this;
    }

    /**
     * @return the completionDate
     */

    public String getCompletionDate() {
        return completionDate;
    }

    /**
     * @param completionDate
     *            the completionDate to set
     *
     * @return this instance for for chaining
     */

    public ParsedTenderLot setCompletionDate(final String completionDate) {
        this.completionDate = completionDate;
        return this;
    }

    /**
     * @return the bids
     */

    public List<ParsedBid> getBids() {
        return bids;
    }

    /**
     * @param bids
     *            the bids to set
     *
     * @return this instance for for chaining
     */

    public ParsedTenderLot setBids(final List<ParsedBid> bids) {
        this.bids = bids;
        return this;
    }

    /**
     * @return the bidsCount
     */

    public String getBidsCount() {
        return bidsCount;
    }

    /**
     * @param bidsCount
     *            the bidsCount to set
     *
     * @return this instance for for chaining
     */

    public ParsedTenderLot setBidsCount(final String bidsCount) {
        this.bidsCount = bidsCount;
        return this;
    }

    /**
     * @return the validBidsCount
     */

    public String getValidBidsCount() {
        return validBidsCount;
    }

    /**
     * @param validBidsCount
     *            the validBidsCount to set
     *
     * @return this instance for for chaining
     */

    public ParsedTenderLot setValidBidsCount(final String validBidsCount) {
        this.validBidsCount = validBidsCount;
        return this;
    }

    /**
     * @return the electronicBidsCount
     */

    public String getElectronicBidsCount() {
        return electronicBidsCount;
    }

    /**
     * @param electronicBidsCount
     *            the electronicBidsCount to set
     *
     * @return this instance for for chaining
     */

    public ParsedTenderLot setElectronicBidsCount(final String electronicBidsCount) {
        this.electronicBidsCount = electronicBidsCount;
        return this;
    }

    /**
     * @return the smeBidsCount
     */

    public String getSmeBidsCount() {
        return smeBidsCount;
    }

    /**
     * @param smeBidsCount
     *            the smeBidsCount to set
     *
     * @return this instance for for chaining
     */

    public ParsedTenderLot setSmeBidsCount(final String smeBidsCount) {
        this.smeBidsCount = smeBidsCount;
        return this;
    }

    /**
     * @return the otherEuMemberStatesCompaniesBidsCount
     */

    public String getOtherEuMemberStatesCompaniesBidsCount() {
        return otherEuMemberStatesCompaniesBidsCount;
    }

    /**
     * @param otherEuMemberStatesCompaniesBidsCount
     *            the otherEuMemberStatesCompaniesBidsCount to set
     *
     * @return this instance for for chaining
     */

    public ParsedTenderLot setOtherEuMemberStatesCompaniesBidsCount(
            final String otherEuMemberStatesCompaniesBidsCount) {
        this.otherEuMemberStatesCompaniesBidsCount = otherEuMemberStatesCompaniesBidsCount;
        return this;
    }

    /**
     * @return the nonEuMemberStatesCompaniesBidsCount
     */

    public String getNonEuMemberStatesCompaniesBidsCount() {
        return nonEuMemberStatesCompaniesBidsCount;
    }

    /**
     * @param nonEuMemberStatesCompaniesBidsCount
     *            the nonEuMemberStatesCompaniesBidsCount to set
     *
     * @return this instance for for chaining
     */

    public ParsedTenderLot setNonEuMemberStatesCompaniesBidsCount(final String nonEuMemberStatesCompaniesBidsCount) {
        this.nonEuMemberStatesCompaniesBidsCount = nonEuMemberStatesCompaniesBidsCount;
        return this;
    }

    /**
     * @return number of foreign participants
     */

    public String getForeignCompaniesBidsCount() {
        return foreignCompaniesBidsCount;
    }

    /**
     * @param foreignCompaniesBidsCount
     *            number of foreign participants
     *
     * @return this instance for chaining
     */

    public ParsedTenderLot setForeignCompaniesBidsCount(final String foreignCompaniesBidsCount) {
        this.foreignCompaniesBidsCount = foreignCompaniesBidsCount;
        return this;
    }

    /**
     * Adds bid to the list of bids or create a new list with given bid if none
     * exists.
     *
     * @param bid
     *            new bid to be added
     *
     * @return this instance for chaining
     */
    public ParsedTenderLot addBid(final ParsedBid bid) {
        if (bid != null) {
            if (getBids() == null) {
                setBids(new ArrayList<>());
            }
            this.bids.add(bid);
        }

        return this;
    }

    /**
     * Return lot id.
     * 
     * @return lot id
     */
    public String getLotId() {
        return lotId;
    }

    /**
     * Sets lot id.
     * 
     * @param lotId
     *            lot id
     * @return self
     */
    public ParsedTenderLot setLotId(final String lotId) {
        this.lotId = lotId;
        return this;
    }
}
