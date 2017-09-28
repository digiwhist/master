package eu.dl.dataaccess.dto.parsed;

import java.util.ArrayList;
import java.util.List;

/**
 * Bid.
 */
public class ParsedBid {
    /**
     * Winning bid.
     */
    private String isWinning;

    /**
     * Bid was disqualified.
     */
    private String isDisqualified;

    /**
     * Stated reason for disqualification.
     */
    private String disqualificationReason;

    /**
     * Bidders.
     */
    private List<ParsedBody> bidders;

    /**
     * List of offered unit prices.
     */
    private List<ParsedUnitPrice> unitPrices;

    /**
     * List of associated documents, such as bid, final contract or document
     * describing ownership structure.
     */
    private List<ParsedDocument> documents;

    /**
     * Ex post buyer evaluation of quality.
     */
    private String wasInRequestedQuality;

    /**
     * Based on completionTime, or may be ex post buyer evaluation of time.
     */
    private String wasFinishedOnTime;

    /**
     * Based on payment and bidPrice, or ex post evaluation by buyer.
     */
    private String wasForEstimatedValue;

    /**
     * List of actual payments.
     */
    private List<ParsedPayment> payments;

    /**
     * Subcontractor is involved.
     */
    private String isSubcontracted;

    /**
     * Percentage of contract volume to be subcontracted.
     */
    private String subcontractedProportion;

    /**
     * Value of the tender likely to be subcontracted to third parties.
     */
    private ParsedPrice subcontractedValue;

    /**
     * Offered price.
     */
    private ParsedPrice price;

    /**
     * Final calculated price (only for winning bid), calculated according to the algorithm described on
     * zindex wiki (http://wiki.zindex.cz/doku.php?id=internal:urceni_ceny).
     */
    private ParsedPrice robustPrice;

    /**
     * Number of years for annual price.
     */
    private String annualPriceYearsCount;

    /**
     * Number of months for monthly price.
     */
    private String monthlyPriceMonthsCount;

    /**
     * Ad hoc consortium formed by multiple bodies. If true, leading body is
     * specified as bidder.
     */
    private String isConsortium;

    /**
     * List of all subcontractors, includint the indirect ones.
     */
    private List<ParsedBody> subcontractors;

    /**
     * Bid id. This one must be unique within superior lot object.
     */
    private String bidId;
    
    /**
     * Gets the checks if is winning.
     *
     * @return the checks if is winning
     */
    public final String getIsWinning() {
        return isWinning;
    }

    /**
     * Sets the is winning.
     *
     * @param isWinning
     *            the is winning
     * @return the parsed bid
     */
    public final ParsedBid setIsWinning(final String isWinning) {
        this.isWinning = isWinning;
        return this;
    }

    /**
     * Gets the checks if is disqualified.
     *
     * @return the checks if is disqualified
     */
    public final String getIsDisqualified() {
        return isDisqualified;
    }

    /**
     * Sets the is disqualified.
     *
     * @param isDisqualified
     *            the is disqualified
     * @return the parsed bid
     */
    public final ParsedBid setIsDisqualified(final String isDisqualified) {
        this.isDisqualified = isDisqualified;
        return this;
    }

    /**
     * Gets the disqualification reason.
     *
     * @return the disqualification reason
     */
    public final String getDisqualificationReason() {
        return disqualificationReason;
    }

    /**
     * Sets the disqualification reason.
     *
     * @param disqualificationReason
     *            the disqualification reason
     * @return the parsed bid
     */
    public final ParsedBid setDisqualificationReason(final String disqualificationReason) {
        this.disqualificationReason = disqualificationReason;
        return this;
    }

    /**
     * Gets the bidders.
     *
     * @return the bidders
     */
    public final List<ParsedBody> getBidders() {
        return bidders;
    }

    /**
     * Sets the bidders.
     *
     * @param bidders
     *            the bidders
     * @return the parsed bid
     */
    public final ParsedBid setBidders(final List<ParsedBody> bidders) {
        this.bidders = bidders;
        return this;
    }

    /**
     * Gets the unit prices.
     *
     * @return the unit prices
     */
    public final List<ParsedUnitPrice> getUnitPrices() {
        return unitPrices;
    }

    /**
     * Sets the unit prices.
     *
     * @param unitPrices
     *            the unit prices
     * @return the parsed bid
     */
    public final ParsedBid setUnitPrices(final List<ParsedUnitPrice> unitPrices) {
        this.unitPrices = unitPrices;
        return this;
    }

    /**
     * Gets the documents.
     *
     * @return the documents
     */
    public final List<ParsedDocument> getDocuments() {
        return documents;
    }

    /**
     * Sets the documents.
     *
     * @param documents
     *            the documents
     * @return the parsed bid
     */
    public final ParsedBid setDocuments(final List<ParsedDocument> documents) {
        this.documents = documents;
        return this;
    }

    /**
     * Gets the was in requested quality.
     *
     * @return the was in requested quality
     */
    public final String getWasInRequestedQuality() {
        return wasInRequestedQuality;
    }

    /**
     * Sets the was in requested quality.
     *
     * @param wasInRequestedQuality
     *            the was in requested quality
     * @return the parsed bid
     */
    public final ParsedBid setWasInRequestedQuality(final String wasInRequestedQuality) {
        this.wasInRequestedQuality = wasInRequestedQuality;
        return this;
    }

    /**
     * Gets the was finished on time.
     *
     * @return the was finished on time
     */
    public final String getWasFinishedOnTime() {
        return wasFinishedOnTime;
    }

    /**
     * Sets the was finished on time.
     *
     * @param wasFinishedOnTime
     *            the was finished on time
     * @return the parsed bid
     */
    public final ParsedBid setWasFinishedOnTime(final String wasFinishedOnTime) {
        this.wasFinishedOnTime = wasFinishedOnTime;
        return this;
    }

    /**
     * Gets the was for estimated value.
     *
     * @return the was for estimated value
     */
    public final String getWasForEstimatedValue() {
        return wasForEstimatedValue;
    }

    /**
     * Sets the was for estimated value.
     *
     * @param wasForEstimatedValue
     *            the was for estimated value
     * @return the parsed bid
     */
    public final ParsedBid setWasForEstimatedValue(final String wasForEstimatedValue) {
        this.wasForEstimatedValue = wasForEstimatedValue;
        return this;
    }

    /**
     * Gets the payments.
     *
     * @return the payments
     */
    public final List<ParsedPayment> getPayments() {
        return payments;
    }

    /**
     * Sets the payments.
     *
     * @param payments
     *            the payments
     * @return the parsed bid
     */
    public final ParsedBid setPayments(final List<ParsedPayment> payments) {
        this.payments = payments;
        return this;
    }

    /**
     * Adds the payment.
     *
     * @param payment
     *            the payment
     * @return the parsed bid
     */
    public final ParsedBid addPayment(final ParsedPayment payment) {
        if (payment != null) {
            if (getPayments() == null) {
                setPayments(new ArrayList<>());
            }

            this.payments.add(payment);
        }

        return this;
    }

    /**
     * Gets the checks if is subcontracted.
     *
     * @return the checks if is subcontracted
     */
    public final String getIsSubcontracted() {
        return isSubcontracted;
    }

    /**
     * Sets the is subcontracted.
     *
     * @param isSubcontracted
     *            the is subcontracted
     * @return the parsed bid
     */
    public final ParsedBid setIsSubcontracted(final String isSubcontracted) {
        this.isSubcontracted = isSubcontracted;
        return this;
    }

    /**
     * Gets the subcontracted proportion.
     *
     * @return the subcontracted proportion
     */
    public final String getSubcontractedProportion() {
        return subcontractedProportion;
    }

    /**
     * Sets the subcontracted proportion.
     *
     * @param subcontractedProportion
     *            the subcontracted proportion
     * @return the parsed bid
     */
    public final ParsedBid setSubcontractedProportion(final String subcontractedProportion) {
        this.subcontractedProportion = subcontractedProportion;
        return this;
    }

    /**
     * Gets the subcontracted value.
     *
     * @return the subcontracted value
     */
    public final ParsedPrice getSubcontractedValue() {
        return subcontractedValue;
    }

    /**
     * Sets the subcontracted value.
     *
     * @param subcontractedValue
     *            the subcontracted value
     * @return the parsed bid
     */
    public final ParsedBid setSubcontractedValue(final ParsedPrice subcontractedValue) {
        this.subcontractedValue = subcontractedValue;
        return this;
    }

    /**
     * Gets the price.
     *
     * @return the price
     */
    public final ParsedPrice getPrice() {
        return price;
    }

    /**
     * Sets the price.
     *
     * @param price
     *            the price
     * @return the parsed bid
     */
    public final ParsedBid setPrice(final ParsedPrice price) {
        this.price = price;
        return this;
    }

    /**
     * Gets the robust price.
     *
     * @return the robust price
     */
    public final ParsedPrice getRobustPrice() {
        return robustPrice;
    }

    /**
     * Sets the robust price.
     *
     * @param robustPrice
     *            the robust price
     * @return the parsed bid
     */
    public final ParsedBid setRobustPrice(final ParsedPrice robustPrice) {
        this.robustPrice = robustPrice;
        return this;
    }

    /**
     * Gets the annual price years count.
     *
     * @return the annual price years count
     */
    public final String getAnnualPriceYearsCount() {
        return annualPriceYearsCount;
    }

    /**
     * Sets the annual price years count.
     *
     * @param annualPriceYearsCount
     *            the annual price years count
     * @return the parsed bid
     */
    public final ParsedBid setAnnualPriceYearsCount(final String annualPriceYearsCount) {
        this.annualPriceYearsCount = annualPriceYearsCount;
        return this;
    }

    /**
     * Gets the monthly price months count.
     *
     * @return the monthly price months count
     */
    public final String getMonthlyPriceMonthsCount() {
        return monthlyPriceMonthsCount;
    }

    /**
     * Sets the monthly price months count.
     *
     * @param monthlyPriceMonthsCount
     *            the monthly price months count
     * @return the parsed bid
     */
    public final ParsedBid setMonthlyPriceMonthsCount(final String monthlyPriceMonthsCount) {
        this.monthlyPriceMonthsCount = monthlyPriceMonthsCount;
        return this;
    }

    /**
     * Gets the checks if is consortium.
     *
     * @return the checks if is consortium
     */
    public final String getIsConsortium() {
        return isConsortium;
    }

    /**
     * Sets the is consortium.
     *
     * @param isConsortium
     *            the is consortium
     * @return the parsed bid
     */
    public final ParsedBid setIsConsortium(final String isConsortium) {
        this.isConsortium = isConsortium;
        return this;
    }

    /**
     * Gets the subcontractors.
     *
     * @return the subcontractors
     */
    public final List<ParsedBody> getSubcontractors() {
        return subcontractors;
    }

    /**
     * Sets the subcontractors.
     *
     * @param subcontractors
     *            the subcontractors
     * @return the parsed bid
     */
    public final ParsedBid setSubcontractors(final List<ParsedBody> subcontractors) {
        this.subcontractors = subcontractors;
        return this;
    }

    /**
     * Adds the bidder.
     *
     * @param bidder
     *            the bidder
     * @return the parsed bid
     */
    public final ParsedBid addBidder(final ParsedBody bidder) {
        if (bidder != null) {
            if (getBidders() == null) {
                setBidders(new ArrayList<>());
            }

            this.bidders.add(bidder);
        }

        return this;
    }

    /**
     * Adds the unit price.
     *
     * @param unitPrice
     *            the unit price
     * @return the parsed bid
     */
    public final ParsedBid addUnitPrice(final ParsedUnitPrice unitPrice) {
        if (unitPrice != null) {
            if (getUnitPrices() == null) {
                setUnitPrices(new ArrayList<>());
            }

            this.unitPrices.add(unitPrice);
        }

        return this;
    }

    /**
     * Gets the bid id.
     *
     * @return the bid id
     */
    public final String getBidId() {
        return bidId;
    }

    /**
     * Sets the bid id.
     *
     * @param bidId
     *            the bid id
     * @return the parsed bid
     */
    public final ParsedBid setBidId(final String bidId) {
        this.bidId = bidId;
        return this;
    }
}
