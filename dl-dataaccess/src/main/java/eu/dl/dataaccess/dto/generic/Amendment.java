package eu.dl.dataaccess.dto.generic;

import java.time.LocalDate;
import java.util.List;

/**
 * Corrigendum.
 */
public class Amendment {
    /**
     * Cpv codes of the subject.
     */
    private List<CPV> cpvs;

    /**
     * The exact address of the Tender/Lot performance.
     */
    private Address addressOfImplementation;

    /**
     * Subject description.
     */
    private String description;

    /**
     * Estimated date of Tender/Lot start.
     */
    private LocalDate estimatedStartDate;

    /**
     * Estimated date of Tender/Lot end.
     */
    private LocalDate estimatedCompletionDate;

    /**
     * Estimated tender duration in years.
     */
    private Integer estimatedDurationInYears;

    /**
     * Estimated tender duration in months.
     */
    private Integer estimatedDurationInMonths;

    /**
     * Estimated tender duration in days.
     */
    private Integer estimatedDurationInDays;

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
    private Price originalPrice;

    /**
     * Final price value of contract.
     */
    private Price updatedPrice;

    /**
     * Justification for framework agreement going over 4 years.
     */
    private String excessiveFrameworkAgreementJustification;

    /**
     * Identifier of publication on source.
     */
    private String sourceId;



    /**
     * @return the cpvs
     */
    public final List<CPV> getCpvs() {
        return cpvs;
    }

    /**
     * @param newCpvs the cpvs to set
     * @return instance of {@code T} class for chaining
     */
    public final Amendment setCpvs(final List<CPV> newCpvs) {
        this.cpvs = newCpvs;
        return this;
    }

    /**
     * @return the addressOfImplementation
     */
    public final Address getAddressOfImplementation() {
        return addressOfImplementation;
    }

    /**
     * @param newAddressOfImplementation the addressOfImplementation to set
     * @return instance of {@code T} class for chaining
     */
    public final Amendment setAddressOfImplementation(final Address newAddressOfImplementation) {
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
    public final Amendment setDescription(final String newDescription) {
        this.description = newDescription;
        return this;
    }

    /**
     * @return the estimatedStartDate
     */
    public final LocalDate getEstimatedStartDate() {
        return estimatedStartDate;
    }

    /**
     * @param newEstimatedStartDate the estimatedStartDate to set
     * @return instance of {@code T} class for chaining
     */
    public final Amendment setEstimatedStartDate(final LocalDate newEstimatedStartDate) {
        this.estimatedStartDate = newEstimatedStartDate;
        return this;
    }

    /**
     * @return the estimatedCompletionDate
     */
    public final LocalDate getEstimatedCompletionDate() {
        return estimatedCompletionDate;
    }

    /**
     * @param newEstimatedCompletionDate the estimatedCompletionDate to set
     * @return instance of {@code T} class for chaining
     */
    public final Amendment setEstimatedCompletionDate(final LocalDate newEstimatedCompletionDate) {
        this.estimatedCompletionDate = newEstimatedCompletionDate;
        return this;
    }

    /**
     * @return estimated Tender/Lot duration in years
     */
    public final Integer getEstimatedDurationInYears() {
        return estimatedDurationInYears;
    }

    /**
     * @param newEstimatedDurationInYears estimated duration of Tender/Lot in years
     * @return instance of {@code T} class for chaining
     */
    public final Amendment setEstimatedDurationInYears(final Integer newEstimatedDurationInYears) {
        this.estimatedDurationInYears = newEstimatedDurationInYears;
        return this;
    }

    /**
     * @return estimated Tender/Lot duration in months
     */
    public final Integer getEstimatedDurationInMonths() {
        return estimatedDurationInMonths;
    }

    /**
     * @param newEstimatedDurationInMonths estimated duration of Tender/Lot in months
     * @return instance of {@code T} class for chaining
     */
    public final Amendment setEstimatedDurationInMonths(final Integer newEstimatedDurationInMonths) {
        this.estimatedDurationInMonths = newEstimatedDurationInMonths;
        return this;
    }

    /**
     * @return estimated Tender/Lot duration in days
     */
    public final Integer getEstimatedDurationInDays() {
        return estimatedDurationInDays;
    }

    /**
     * @param newEstimatedDurationInDays estimated duration of Tender/Lot in days
     * @return instance of {@code T} class for chaining
     */
    public final Amendment setEstimatedDurationInDays(final Integer newEstimatedDurationInDays) {
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
    public final Amendment setModificationReason(final String modificationReason) {
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
    public final Amendment setModificationReasonShortDescription(final String modificationReasonShortDescription) {
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
    public final Amendment setModificationReasonDescription(final String modificationReasonDescription) {
        this.modificationReasonDescription = modificationReasonDescription;
        return this;
    }

    /**
     * @return the originalPrice
     */
    public final Price getOriginalPrice() {
        return originalPrice;
    }

    /**
     * @param originalPrice
     *         the updatedPrice to set
     *
     * @return this instance for chaining
     */
    public final Amendment setOriginalPrice(final Price originalPrice) {
        this.originalPrice = originalPrice;
        return this;
    }

    /**
     * @return the updatedPrice
     */
    public final Price getUpdatedPrice() {
        return updatedPrice;
    }

    /**
     * @param updatedPrice
     *         the updatedPrice to set
     *
     * @return this instance for chaining
     */
    public final Amendment setUpdatedPrice(final Price updatedPrice) {
        this.updatedPrice = updatedPrice;
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
    public final Amendment setExcessiveFrameworkAgreementJustification(
            final String excessiveFrameworkAgreementJustification) {
        this.excessiveFrameworkAgreementJustification = excessiveFrameworkAgreementJustification;
        return this;
    }

    /**
     * Gets the source id.
     *
     * @return the source id
     */
    public final String getSourceId() {
        return sourceId;
    }

    /**
     * Sets the source id.
     *
     * @param sourceId
     *            the source id
     * @return the Amendment
     */
    public final Amendment setSourceId(final String sourceId) {
        this.sourceId = sourceId;
        return this;
    }
}
