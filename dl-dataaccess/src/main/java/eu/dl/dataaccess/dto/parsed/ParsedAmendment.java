package eu.dl.dataaccess.dto.parsed;

import java.util.List;

/**
 * Corrigendum.
 */
public class ParsedAmendment {
    /**
     * Cpv codes of the subject.
     */
    private List<ParsedCPV> cpvs;

    /**
     * The exact address of the Tender/Lot performance.
     */
    private ParsedAddress addressOfImplementation;

    /**
     * Subject description.
     */
    private String description;

    /**
     * Estimated date of Tender/Lot start.
     */
    private String estimatedStartDate;

    /**
     * Estimated date of Tender/Lot end.
     */
    private String estimatedCompletionDate;

    /**
     * Estimated tender duration in years.
     */
    private String estimatedDurationInYears;

    /**
     * Estimated tender duration in months.
     */
    private String estimatedDurationInMonths;

    /**
     * Estimated tender duration in days.
     */
    private String estimatedDurationInDays;

    /**
     * Reason for modification.
     */
    private String modificationReason;

    /**
     * Short description of modification reason.
     */
    private String modificationReasonShortDescription;

    /**
     * Detailed description of modification reason.
     */
    private String modificationReasonDescription;

    /**
     * Original price value of contract.
     */
    private ParsedPrice originalPrice;

    /**
     * Final price value of contract.
     */
    private ParsedPrice finalPrice;

    /**
     * Justification for framework agreement going over 4 years.
     */
    private String excessiveFrameworkAgreementJustification;


    
    

    /**
     * @return the cpvs
     */
    public final List<ParsedCPV> getCpvs() {
        return cpvs;
    }

    /**
     * @param newCpvs the cpvs to set
     * @return instance of {@code T} class for chaining
     */
    public final ParsedAmendment setCpvs(final List<ParsedCPV> newCpvs) {
        this.cpvs = newCpvs;
        return this;
    }

    /**
     * @return the addressOfImplementation
     */
    public final ParsedAddress getAddressOfImplementation() {
        return addressOfImplementation;
    }

    /**
     * @param newAddressOfImplementation the addressOfImplementation to set
     * @return instance of {@code T} class for chaining
     */
    public final ParsedAmendment setAddressOfImplementation(final ParsedAddress newAddressOfImplementation) {
        this.addressOfImplementation = newAddressOfImplementation;
        return this;
    }

    /**
     * @return the description
     */
    public final String getDescription() {
        return description;
    }

    /**
     * @param newDescription the description to set
     * @return instance of {@code T} class for chaining
     */
    public final ParsedAmendment setDescription(final String newDescription) {
        this.description = newDescription;
        return this;
    }

    /**
     * @return the estimatedStartDate
     */
    public final String getEstimatedStartDate() {
        return estimatedStartDate;
    }

    /**
     * @param newEstimatedStartDate the estimatedStartDate to set
     * @return instance of {@code T} class for chaining
     */
    public final ParsedAmendment setEstimatedStartDate(final String newEstimatedStartDate) {
        this.estimatedStartDate = newEstimatedStartDate;
        return this;
    }

    /**
     * @return the estimatedCompletionDate
     */
    public final String getEstimatedCompletionDate() {
        return estimatedCompletionDate;
    }

    /**
     * @param newEstimatedCompletionDate the estimatedCompletionDate to set
     * @return instance of {@code T} class for chaining
     */
    public final ParsedAmendment setEstimatedCompletionDate(final String newEstimatedCompletionDate) {
        this.estimatedCompletionDate = newEstimatedCompletionDate;
        return this;
    }

    /**
     * @return estimated Tender/Lot duration in years
     */
    public final String getEstimatedDurationInYears() {
        return estimatedDurationInYears;
    }

    /**
     * @param newEstimatedDurationInYears estimated duration of Tender/Lot in years
     * @return instance of {@code T} class for chaining
     */
    public final ParsedAmendment setEstimatedDurationInYears(final String newEstimatedDurationInYears) {
        this.estimatedDurationInYears = newEstimatedDurationInYears;
        return this;
    }

    /**
     * @return estimated Tender/Lot duration in months
     */
    public final String getEstimatedDurationInMonths() {
        return estimatedDurationInMonths;
    }

    /**
     * @param newEstimatedDurationInMonths estimated duration of Tender/Lot in months
     * @return instance of {@code T} class for chaining
     */
    public final ParsedAmendment setEstimatedDurationInMonths(final String newEstimatedDurationInMonths) {
        this.estimatedDurationInMonths = newEstimatedDurationInMonths;
        return this;
    }

    /**
     * @return estimated Tender/Lot duration in days
     */
    public final String getEstimatedDurationInDays() {
        return estimatedDurationInDays;
    }

    /**
     * @param newEstimatedDurationInDays estimated duration of Tender/Lot in days
     * @return instance of {@code T} class for chaining
     */
    public final ParsedAmendment setEstimatedDurationInDays(final String newEstimatedDurationInDays) {
        this.estimatedDurationInDays = newEstimatedDurationInDays;
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
    public final ParsedAmendment setModificationReason(final String modificationReason) {
        this.modificationReason = modificationReason;
        return this;
    }

    /**
     * Gets modificationReasonShortDescription.
     *
     * @return value of modificationReasonShortDescription
     */
    public final String getModificationReasonShortDescription() {
        return modificationReasonShortDescription;
    }

    /**
     * Sets modificationReasonShortDescription.
     *
     * @param modificationReasonShortDescription
     *         the modificationReasonShortDescription to set
     *
     * @return this instance for chaining
     */
    public final ParsedAmendment setModificationReasonShortDescription(final String modificationReasonShortDescription) {
        this.modificationReasonShortDescription = modificationReasonShortDescription;
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
    public final ParsedAmendment setModificationReasonDescription(final String modificationReasonDescription) {
        this.modificationReasonDescription = modificationReasonDescription;
        return this;
    }

    /**
     * @return the originalPrice
     */
    public final ParsedPrice getOriginalPrice() {
        return originalPrice;
    }

    /**
     * @param originalPrice
     *         the finalPrice to set
     *
     * @return this instance for chaining
     */
    public final ParsedAmendment setOriginalPrice(final ParsedPrice originalPrice) {
        this.originalPrice = originalPrice;
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
    public final ParsedAmendment setFinalPrice(final ParsedPrice finalPrice) {
        this.finalPrice = finalPrice;
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
    public final ParsedAmendment setExcessiveFrameworkAgreementJustification(
            final String excessiveFrameworkAgreementJustification) {
        this.excessiveFrameworkAgreementJustification = excessiveFrameworkAgreementJustification;
        return this;
    }
}
