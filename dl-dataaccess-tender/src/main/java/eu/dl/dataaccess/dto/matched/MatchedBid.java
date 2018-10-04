package eu.dl.dataaccess.dto.matched;

import eu.dl.dataaccess.dto.clean.CleanBid;
import eu.dl.dataaccess.dto.codetables.BodyType;
import eu.dl.dataaccess.dto.generic.Document;
import eu.dl.dataaccess.dto.generic.Payment;
import eu.dl.dataaccess.dto.generic.Price;
import eu.dl.dataaccess.dto.generic.UnitPrice;
import eu.dl.dataaccess.dto.utils.InitUtils;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;


/**
 * Bid.
 */
public class MatchedBid implements MasterablePart {
    /**
     * Winning bid.
     */
    private Boolean isWinning;

    /**
     * Bid was disqualified.
     */
    private Boolean isDisqualified;

    /**
     * Stated reason for disqualification.
     */
    private String disqualificationReason;

    /**
     * Bidders.
     */
    private List<MatchedBody> bidders;

    /**
     * List of offered unit prices.
     */
    private List<UnitPrice> unitPrices;

    /**
     * List of associated documents, such as bid, final tender or document
     * describing ownership structure.
     */
    private List<Document> documents;

    /**
     * Ex post buyer evaluation of quality.
     */
    private Boolean wasInRequestedQuality;

    /**
     * Based on completionTime, or may be ex post buyer evaluation of time.
     */
    private Boolean wasFinishedOnTime;

    /**
     * Based on payment and bidPrice, or ex post evaluation by buyer.
     */
    private Boolean wasForEstimatedValue;

    /**
     * List of actual payments.
     */
    private List<Payment> payments;

    /**
     * Subcontractor is involved.
     */
    private Boolean isSubcontracted;

    /**
     * Percentage of tender volume to be subcontracted.
     */
    private Integer subcontractedProportion;

    /**
     * Value of the tender likely to be subcontracted to third parties.
     */
    private Price subcontractedValue;

    /**
     * Offered price.
     */
    private Price price;

    /**
     * Final calculated price (only for winning bid), calculated according to the algorithm described on
     * zindex wiki (http://wiki.zindex.cz/doku.php?id=internal:urceni_ceny).
     */
    private Price robustPrice;

    /**
     * Number of years for annual price.
     */
    private Integer annualPriceYearsCount;

    /**
     * Number of months for monthly price.
     */
    private Integer monthlyPriceMonthsCount;

    /**
     * Ad hoc consortium formed by multiple bodies. If true, leading body is
     * specified as bidder.
     */
    private Boolean isConsortium;

    /**
     * List of all subcontractors, includint the indirect ones.
     */
    private List<MatchedBody> subcontractors;

    /**
     * Bid id. This one must be unique within superior lot object.
     */
    private String bidId;

    /**
     * Structured Bid id.
     */
    private StructuredBidId structuredId;

    /**
     * Publication date.
     */
    private LocalDate publicationDate;

    /**
     * Creates empty matched bid.
     */
    public MatchedBid() {
        super();
    }

    /**
     * Initializes matched bid from clean instance.
     * 
     * @param cleanBid
     *            clean bid
     */
    public MatchedBid(final CleanBid cleanBid) {
        super();
        setIsWinning(cleanBid.getIsWinning());
        setIsDisqualified(cleanBid.getIsDisqualified());
        setDisqualificationReason(cleanBid.getDisqualificationReason());
        setBidders(InitUtils.cleanToMatchedBody(cleanBid.getBidders(), BodyType.BIDDERS));
        setUnitPrices(cleanBid.getUnitPrices());
        setDocuments(cleanBid.getDocuments());
        setWasInRequestedQuality(cleanBid.getWasInRequestedQuality());
        setWasFinishedOnTime(cleanBid.getWasFinishedOnTime());
        setWasForEstimatedValue(cleanBid.getWasForEstimatedValue());
        setPayments(cleanBid.getPayments());
        setIsSubcontracted(cleanBid.getIsSubcontracted());
        setSubcontractedProportion(cleanBid.getSubcontractedProportion());
        setSubcontractedValue(cleanBid.getSubcontractedValue());
        setPrice(cleanBid.getPrice());
        setRobustPrice(cleanBid.getRobustPrice());
        setAnnualPriceYearsCount(cleanBid.getAnnualPriceYearsCount());
        setMonthlyPriceMonthsCount(cleanBid.getMonthlyPriceMonthsCount());
        setIsConsortium(cleanBid.getIsConsortium());
        setSubcontractors(InitUtils.cleanToMatchedBody(cleanBid.getSubcontractors(), BodyType.SUB_CONTRACTORS));
        setBidId(cleanBid.getBidId());
    }

    /**
     * Gets the checks if is winning.
     *
     * @return the isWinning
     */
    public final Boolean getIsWinning() {
        return isWinning;
    }

    /**
     * Sets the is winning.
     *
     * @param isWinning
     *            the isWinning to set
     * @return this instance for chaining
     */
    public final MatchedBid setIsWinning(final Boolean isWinning) {
        this.isWinning = isWinning;
        return this;
    }

    /**
     * Gets the checks if is disqualified.
     *
     * @return the isDisqualified
     */
    public final Boolean getIsDisqualified() {
        return isDisqualified;
    }

    /**
     * Sets the is disqualified.
     *
     * @param isDisqualified
     *            the isDisqualified to set
     * @return this instance for chaining
     */
    public final MatchedBid setIsDisqualified(final Boolean isDisqualified) {
        this.isDisqualified = isDisqualified;
        return this;
    }

    /**
     * Gets the disqualification reason.
     *
     * @return the disqualificationReason
     */
    public final String getDisqualificationReason() {
        return disqualificationReason;
    }

    /**
     * Sets the disqualification reason.
     *
     * @param disqualificationReason
     *            the disqualificationReason to set
     * @return this instance for chaining
     */
    public final MatchedBid setDisqualificationReason(final String disqualificationReason) {
        this.disqualificationReason = disqualificationReason;
        return this;
    }

    /**
     * Gets the bidders.
     *
     * @return the list of bidders
     */
    public final List<MatchedBody> getBidders() {
        return bidders;
    }

    /**
     * Sets the bidders.
     *
     * @param bidders
     *            the list of bidders to set
     * @return this instance for chaining
     */
    public final MatchedBid setBidders(final List<MatchedBody> bidders) {
        this.bidders = bidders;
        return this;
    }

    /**
     * Adds the bidder.
     *
     * @param bidder
     *            the bidder
     * @return this instance for chaining
     */
    public final MatchedBid addBidder(final MatchedBody bidder) {
        if (bidder != null) {
            if (getBidders() == null) {
                setBidders(new ArrayList<>());
            }

            this.bidders.add(bidder);
        }

        return this;
    }

    /**
     * Gets the unit prices.
     *
     * @return the unitPrices
     */
    public final List<UnitPrice> getUnitPrices() {
        return unitPrices;
    }

    /**
     * Sets the unit prices.
     *
     * @param unitPrices
     *            the unitPrices to set
     * @return this instance for chaining
     */
    public final MatchedBid setUnitPrices(final List<UnitPrice> unitPrices) {
        this.unitPrices = unitPrices;
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
     * @param documents
     *            the documents to set
     * @return this instance for chaining
     */
    public final MatchedBid setDocuments(final List<Document> documents) {
        this.documents = documents;
        return this;
    }

    /**
     * Gets the was in requested quality.
     *
     * @return the wasInRequestedQuality
     */
    public final Boolean getWasInRequestedQuality() {
        return wasInRequestedQuality;
    }

    /**
     * Sets the was in requested quality.
     *
     * @param wasInRequestedQuality
     *            the wasInRequestedQuality to set
     * @return this instance for chaining
     */
    public final MatchedBid setWasInRequestedQuality(final Boolean wasInRequestedQuality) {
        this.wasInRequestedQuality = wasInRequestedQuality;
        return this;
    }

    /**
     * Gets the was finished on time.
     *
     * @return the wasFinishedOnTime
     */
    public final Boolean getWasFinishedOnTime() {
        return wasFinishedOnTime;
    }

    /**
     * Sets the was finished on time.
     *
     * @param wasFinishedOnTime
     *            the wasFinishedOnTime to set
     * @return this instance for chaining
     */
    public final MatchedBid setWasFinishedOnTime(final Boolean wasFinishedOnTime) {
        this.wasFinishedOnTime = wasFinishedOnTime;
        return this;
    }

    /**
     * Gets the was for estimated value.
     *
     * @return the wasForEstimatedValue
     */
    public final Boolean getWasForEstimatedValue() {
        return wasForEstimatedValue;
    }

    /**
     * Sets the was for estimated value.
     *
     * @param wasForEstimatedValue
     *            the wasForEstimatedValue to set
     * @return this instance for chaining
     */
    public final MatchedBid setWasForEstimatedValue(final Boolean wasForEstimatedValue) {
        this.wasForEstimatedValue = wasForEstimatedValue;
        return this;
    }

    /**
     * Gets the payments.
     *
     * @return the payments
     */
    public final List<Payment> getPayments() {
        return payments;
    }

    /**
     * Sets the payments.
     *
     * @param payments
     *            the payments to set
     * @return this instance for chaining
     */
    public final MatchedBid setPayments(final List<Payment> payments) {
        this.payments = payments;
        return this;
    }

    /**
     * Gets the checks if is subcontracted.
     *
     * @return the isSubcontracted
     */
    public final Boolean getIsSubcontracted() {
        return isSubcontracted;
    }

    /**
     * Sets the is subcontracted.
     *
     * @param isSubcontracted
     *            the isSubcontracted to set
     * @return this instance for chaining
     */
    public final MatchedBid setIsSubcontracted(final Boolean isSubcontracted) {
        this.isSubcontracted = isSubcontracted;
        return this;
    }

    /**
     * Gets the subcontracted proportion.
     *
     * @return the subcontractedProportion
     */
    public final Integer getSubcontractedProportion() {
        return subcontractedProportion;
    }

    /**
     * Sets the subcontracted proportion.
     *
     * @param subcontractedProportion
     *            the subcontractedProportion to set
     * @return this instance for chaining
     */
    public final MatchedBid setSubcontractedProportion(final Integer subcontractedProportion) {
        this.subcontractedProportion = subcontractedProportion;
        return this;
    }

    /**
     * Gets the subcontracted value.
     *
     * @return value of the tender likely to be subcontracted to third parties
     */
    public final Price getSubcontractedValue() {
        return subcontractedValue;
    }

    /**
     * Sets the subcontracted value.
     *
     * @param subcontractedValue
     *            value of the tender likely to be subcontracted to third
     *            parties
     * @return this instance for chaining
     */
    public final MatchedBid setSubcontractedValue(final Price subcontractedValue) {
        this.subcontractedValue = subcontractedValue;
        return this;
    }

    /**
     * Gets the price.
     *
     * @return the price
     */
    public final Price getPrice() {
        return price;
    }

    /**
     * Sets the price.
     *
     * @param price
     *            the price to set
     * @return this instance for chaining
     */
    public final MatchedBid setPrice(final Price price) {
        this.price = price;
        return this;
    }

    /**
     * Gets the robust price.
     *
     * @return the robust price
     */
    public final Price getRobustPrice() {
        return robustPrice;
    }

    /**
     * Sets the robust price.
     *
     * @param robustPrice
     *            the robust price
     * @return the matched bid
     */
    public final MatchedBid setRobustPrice(final Price robustPrice) {
        this.robustPrice = robustPrice;
        return this;
    }

    /**
     * Gets the annual price years count.
     *
     * @return number of years for annual price
     */
    public final Integer getAnnualPriceYearsCount() {
        return annualPriceYearsCount;
    }

    /**
     * Sets the annual price years count.
     *
     * @param annualPriceYearsCount
     *            number of years for annual price
     * @return this instance for chaining
     */
    public final MatchedBid setAnnualPriceYearsCount(final Integer annualPriceYearsCount) {
        this.annualPriceYearsCount = annualPriceYearsCount;
        return this;
    }

    /**
     * Gets the monthly price months count.
     *
     * @return number of months for monthly price
     */
    public final Integer getMonthlyPriceMonthsCount() {
        return monthlyPriceMonthsCount;
    }

    /**
     * Sets the monthly price months count.
     *
     * @param monthlyPriceMonthsCount
     *            number of months for monthly price
     * @return this instance for chaining
     */
    public final MatchedBid setMonthlyPriceMonthsCount(final Integer monthlyPriceMonthsCount) {
        this.monthlyPriceMonthsCount = monthlyPriceMonthsCount;
        return this;
    }

    /**
     * Gets the checks if is consortium.
     *
     * @return the isConsortium
     */
    public final Boolean getIsConsortium() {
        return isConsortium;
    }

    /**
     * Sets the is consortium.
     *
     * @param isConsortium
     *            the isConsortium to set
     * @return this instance for chaining
     */
    public final MatchedBid setIsConsortium(final Boolean isConsortium) {
        this.isConsortium = isConsortium;
        return this;
    }

    /**
     * Gets the subcontractors.
     *
     * @return the subcontractors
     */
    public final List<MatchedBody> getSubcontractors() {
        return subcontractors;
    }

    /**
     * Sets the subcontractors.
     *
     * @param subcontractors
     *            the subcontractors to set
     * @return this instance for chaining
     */
    public final MatchedBid setSubcontractors(final List<MatchedBody> subcontractors) {
        this.subcontractors = subcontractors;
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
     * @return the matched bid
     */
    public final MatchedBid setBidId(final String bidId) {
        this.bidId = bidId;
        return this;
    }

    /**
     * Gets structuredId.
     *
     * @return value of structuredId
     */
    public final StructuredBidId getStructuredId() {
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
    public final MatchedBid setStructuredId(final StructuredBidId structuredId) {
        this.structuredId = structuredId;
        return this;
    }

    @Override
    public final String getTenderId() {
        if (getStructuredId() == null) {
            return null;
        }
        
        return getStructuredId().getTenderId();
    }

    @Override
    public final LocalDate getPublicationDate() {
        return publicationDate;
    }

    /**
     * @param newPublicationDate
     *            the tender id to set
     * @return this instance for chaining
     */
    public final MatchedBid setPublicationDate(final LocalDate newPublicationDate) {
        this.publicationDate = newPublicationDate;
        return this;
    }

    @Override
    public final LocalDateTime getCreatedRaw() {
        return null;
    }
}
