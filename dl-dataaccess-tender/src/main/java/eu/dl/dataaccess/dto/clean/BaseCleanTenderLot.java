package eu.dl.dataaccess.dto.clean;

import java.time.LocalDate;
import java.util.List;

import eu.dl.dataaccess.annotation.Transformable;
import eu.dl.dataaccess.dto.codetables.SelectionMethod;
import eu.dl.dataaccess.dto.generic.Address;
import eu.dl.dataaccess.dto.generic.AwardCriterion;
import eu.dl.dataaccess.dto.generic.CPV;
import eu.dl.dataaccess.dto.generic.Funding;
import eu.dl.dataaccess.dto.generic.Price;

/**
 * Abstract clean tender lot DTO containing properties that may also be directly
 * within the tender level.
 *
 * @param <T>
 *         extending class (for fluent interface purposes)
 */
@Transformable
public abstract class BaseCleanTenderLot<T> extends CleanStorableDTO {
    /**
     * Tender/Lot title.
     */
    protected String title;

    /**
     * Tender/Lot title in English.
     */
    protected String titleEnglish;

    /**
     * Subject description.
     */
    protected String description;

    /**
     * Subject description in English.
     */
    protected String descriptionEnglish;

    /**
     * Tender/Lot is awarded.
     */
    protected Boolean isAwarded;

    /**
     * Acceptance of variant offers.
     */
    protected Boolean areVariantsAccepted;

    /**
     * Lot has options.
     */
    protected Boolean hasOptions;

    /**
     * Describes required qualification of bidder.
     */
    protected String eligibilityCriteria;

    /**
     * tender/lot is covered by GPA.
     */
    protected Boolean isCoveredByGpa;

    /**
     * tender/lot is awarded as superior framework agreement.
     */
    protected Boolean isFrameworkAgreement;

    /**
     * Envisaged maximum number of participants to the framework agreement.
     */
    protected Integer maxFrameworkAgreementParticipants;

    /**
     * The exact address of the tender/lot performance.
     */
    protected Address addressOfImplementation;

    /**
     * Is either initiation of dynamic purchasing system, or a purchase via one.
     */
    protected Boolean isDps;

    /**
     * Estimated date of tender/lot start.
     */
    protected LocalDate estimatedStartDate;

    /**
     * Estimated date of tender/lot end.
     */
    protected LocalDate estimatedCompletionDate;

    /**
     * Estimated tender duration in years.
     */
    protected Integer estimatedDurationInYears;

    /**
     * Estimated tender duration in months.
     */
    protected Integer estimatedDurationInMonths;

    /**
     * Estimated tender duration in days.
     */
    protected Integer estimatedDurationInDays;

    /**
     * Date of decision on tender/lot award.
     */
    protected LocalDate awardDecisionDate;

    /**
     * Date of tender signature.
     */
    protected LocalDate contractSignatureDate;

    /**
     * Cpv codes of the subject.
     */
    protected List<CPV> cpvs;

    /**
     * List of tender/lot fundings.
     */
    protected List<Funding> fundings;

    /**
     * Method used for bids evaluation (lowest price or MEAT).
     */
    protected SelectionMethod selectionMethod;

    /**
     * List of awarding criteria.
     */
    protected List<AwardCriterion> awardCriteria;

    /**
     * Is awarded by electronic auction.
     */
    protected Boolean isElectronicAuction;

    /**
     * Date of tender/lot cancellation (or its publication).
     */
    protected LocalDate cancellationDate;

    /**
     * Stated reason for cancellation.
     */
    protected String cancellationReason;

    /**
     * Estimated value of tender/lot.
     */
    protected Price estimatedPrice;

    /**
     * Number of envisaged candidates.
     */
    protected Integer envisagedCandidatesCount;

    /**
     * Min number of envisaged candidates.
     */
    protected Integer envisagedMinCandidatesCount;

    /**
     * Max number of envisaged candidates.
     */
    protected Integer envisagedMaxCandidatesCount;

    /**
     * Objective criteria for choosing the limited number of candidates.
     */
    protected String limitedCandidatesCountCriteria;

    /**
     * @return the title
     */
    public final String getTitle() {
        return title;
    }

    /**
     * @param newTitle
     *         the title to set
     *
     * @return instance of {@code T} class for chaining
     */
    public final T setTitle(final String newTitle) {
        this.title = newTitle;
        return (T) this;
    }

    /**
     * @return the titleEnglish
     */
    public final String getTitleEnglish() {
        return titleEnglish;
    }

    /**
     * @param newTitleEnglish
     *         the titleEnglish to set
     *
     * @return instance of {@code T} class for chaining
     */
    public final T setTitleEnglish(final String newTitleEnglish) {
        this.titleEnglish = newTitleEnglish;
        return (T) this;
    }

    /**
     * @return the description
     */
    public final String getDescription() {
        return description;
    }

    /**
     * @param newDescription
     *         the description to set
     *
     * @return instance of {@code T} class for chaining
     */
    public final T setDescription(final String newDescription) {
        this.description = newDescription;
        return (T) this;
    }

    /**
     * @return the descriptionEnglish
     */
    public final String getDescriptionEnglish() {
        return descriptionEnglish;
    }

    /**
     * @param newDescriptionEnglish
     *         the descriptionEnglish to set
     *
     * @return instance of {@code T} class for chaining
     */
    public final T setDescriptionEnglish(final String newDescriptionEnglish) {
        this.descriptionEnglish = newDescriptionEnglish;
        return (T) this;
    }

    /**
     * @return whether tender/lot is awarded
     */
    public final Boolean getIsAwarded() {
        return isAwarded;
    }

    /**
     * @param newIsAwarded
     *         whether tender/lot is awarded
     *
     * @return this instance for chaining
     */
    public final T setIsAwarded(final Boolean newIsAwarded) {
        this.isAwarded = newIsAwarded;
        return (T) this;
    }

    /**
     * @return true if variant offers are accepted
     */
    public final Boolean getAreVariantsAccepted() {
        return areVariantsAccepted;
    }

    /**
     * @param newAreVariantsAccepted
     *         whether variant offers are accepted
     *
     * @return this instance for chaining
     */
    public final T setAreVariantsAccepted(final Boolean newAreVariantsAccepted) {
        this.areVariantsAccepted = newAreVariantsAccepted;
        return (T) this;
    }

    /**
     * @return whether lot has options
     */
    public final Boolean getHasOptions() {
        return hasOptions;
    }

    /**
     * @param newHasOptions
     *         whether lot has options
     *
     * @return this instance for chaining
     */
    public final T setHasOptions(final Boolean newHasOptions) {
        this.hasOptions = newHasOptions;
        return (T) this;
    }

    /**
     * @return the eligibilityCriteria
     */
    public final String getEligibilityCriteria() {
        return eligibilityCriteria;
    }

    /**
     * @param newEligibilityCriteria
     *         the eligibilityCriteria to set
     *
     * @return this instance for for chaining
     */
    public final T setEligibilityCriteria(final String newEligibilityCriteria) {
        this.eligibilityCriteria = newEligibilityCriteria;
        return (T) this;
    }

    /**
     * @return the isCoveredByGpa
     */
    public final Boolean getIsCoveredByGpa() {
        return isCoveredByGpa;
    }

    /**
     * @param newIsCoveredByGpa
     *         the isCoveredByGpa to set
     *
     * @return instance of {@code T} class for chaining
     */
    public final T setIsCoveredByGpa(final Boolean newIsCoveredByGpa) {
        this.isCoveredByGpa = newIsCoveredByGpa;
        return (T) this;
    }

    /**
     * @return the isFrameworkAgreement
     */
    public final Boolean getIsFrameworkAgreement() {
        return isFrameworkAgreement;
    }

    /**
     * @param newIsFrameworkAgreement
     *         the isFrameworkAgreement to set
     *
     * @return instance of {@code T} class for chaining
     */
    public final T setIsFrameworkAgreement(final Boolean newIsFrameworkAgreement) {
        this.isFrameworkAgreement = newIsFrameworkAgreement;
        return (T) this;
    }

    /**
     * @return the maxFrameworkAgreementParticipants
     */
    public final Integer getMaxFrameworkAgreementParticipants() {
        return maxFrameworkAgreementParticipants;
    }

    /**
     * @param newMaxFrameworkAgreementParticipants
     *         the maxFrameworkAgreementParticipants to set
     *
     * @return instance of {@code T} class for chaining
     */
    public final T setMaxFrameworkAgreementParticipants(final Integer newMaxFrameworkAgreementParticipants) {
        this.maxFrameworkAgreementParticipants = newMaxFrameworkAgreementParticipants;
        return (T) this;
    }

    /**
     * @return the addressOfImplementation
     */
    public final Address getAddressOfImplementation() {
        return addressOfImplementation;
    }

    /**
     * @param newAddressOfImplementation
     *         the addressOfImplementation to set
     *
     * @return instance of {@code T} class for chaining
     */
    public final T setAddressOfImplementation(final Address newAddressOfImplementation) {
        this.addressOfImplementation = newAddressOfImplementation;
        return (T) this;
    }

    /**
     * @return the isDps
     */
    public final Boolean getIsDps() {
        return isDps;
    }

    /**
     * @param newIsDps
     *         the isDps to set
     *
     * @return instance of {@code T} class for chaining
     */
    public final T setIsDps(final Boolean newIsDps) {
        this.isDps = newIsDps;
        return (T) this;
    }

    /**
     * @return the estimatedStartDate
     */
    public final LocalDate getEstimatedStartDate() {
        return estimatedStartDate;
    }

    /**
     * @param newEstimatedStartDate
     *         the estimatedStartDate to set
     *
     * @return instance of {@code T} class for chaining
     */
    public final T setEstimatedStartDate(final LocalDate newEstimatedStartDate) {
        this.estimatedStartDate = newEstimatedStartDate;
        return (T) this;
    }

    /**
     * @return the estimatedCompletionDate
     */
    public final LocalDate getEstimatedCompletionDate() {
        return estimatedCompletionDate;
    }

    /**
     * @param newEstimatedCompletionDate
     *         the estimatedCompletionDate to set
     *
     * @return instance of {@code T} class for chaining
     */
    public final T setEstimatedCompletionDate(final LocalDate newEstimatedCompletionDate) {
        this.estimatedCompletionDate = newEstimatedCompletionDate;
        return (T) this;
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
    public final T setEstimatedDurationInYears(final Integer newEstimatedDurationInYears) {
        this.estimatedDurationInYears = newEstimatedDurationInYears;
        return (T) this;
    }

    /**
     * @return estimated tender/lot duration in months
     */
    public final Integer getEstimatedDurationInMonths() {
        return estimatedDurationInMonths;
    }

    /**
     * @param newEstimatedDurationInMonths
     *         estimated duration of tender/lot in months
     *
     * @return instance of {@code T} class for chaining
     */
    public final T setEstimatedDurationInMonths(final Integer newEstimatedDurationInMonths) {
        this.estimatedDurationInMonths = newEstimatedDurationInMonths;
        return (T) this;
    }

    /**
     * @return estimated tender/lot duration in days
     */
    public final Integer getEstimatedDurationInDays() {
        return estimatedDurationInDays;
    }

    /**
     * @param newEstimatedDurationInDays
     *         estimated duration of tender/lot in days
     *
     * @return instance of {@code T} class for chaining
     */
    public final T setEstimatedDurationInDays(final Integer newEstimatedDurationInDays) {
        this.estimatedDurationInDays = newEstimatedDurationInDays;
        return (T) this;
    }

    /**
     * @return the awardDecisionDate
     */
    public final LocalDate getAwardDecisionDate() {
        return awardDecisionDate;
    }

    /**
     * @param newAwardDecisionDate
     *         the awardDecisionDate to set
     *
     * @return instance of {@code T} class for chaining
     */
    public final T setAwardDecisionDate(final LocalDate newAwardDecisionDate) {
        this.awardDecisionDate = newAwardDecisionDate;
        return (T) this;
    }

    /**
     * @return the contractSignatureDate
     */
    public final LocalDate getContractSignatureDate() {
        return contractSignatureDate;
    }

    /**
     * @param newContractSignatureDate
     *         the contractSignatureDate to set
     *
     * @return instance of {@code T} class for chaining
     */
    public final T setContractSignatureDate(final LocalDate newContractSignatureDate) {
        this.contractSignatureDate = newContractSignatureDate;
        return (T) this;
    }

    /**
     * @return the cpvs
     */
    public final List<CPV> getCpvs() {
        return cpvs;
    }

    /**
     * @param newCpvs
     *         the cpvs to set
     *
     * @return instance of {@code T} class for chaining
     */
    public final T setCpvs(final List<CPV> newCpvs) {
        this.cpvs = newCpvs;
        return (T) this;
    }

    /**
     * @return the fundings
     */
    public final List<Funding> getFundings() {
        return fundings;
    }

    /**
     * @param newFundings
     *         the fundings to set
     *
     * @return instance of {@code T} class for chaining
     */
    public final T setFundings(final List<Funding> newFundings) {
        this.fundings = newFundings;
        return (T) this;
    }

    /**
     * @return the selection method
     */
    public final SelectionMethod getSelectionMethod() {
        return selectionMethod;
    }

    /**
     * @param newSelectionMethod
     *         the selection method to set
     *
     * @return instance of {@code T} class for chaining
     */
    public final T setSelectionMethod(final SelectionMethod newSelectionMethod) {
        this.selectionMethod = newSelectionMethod;
        return (T) this;
    }

    /**
     * @return the awardCriteria
     */
    public final List<AwardCriterion> getAwardCriteria() {
        return awardCriteria;
    }

    /**
     * @param newAwardCriteria
     *         the awardCriteria to set
     *
     * @return instance of {@code T} class for chaining
     */
    public final T setAwardCriteria(final List<AwardCriterion> newAwardCriteria) {
        this.awardCriteria = newAwardCriteria;
        return (T) this;
    }

    /**
     * @return the isElectronicAuction
     */
    public final Boolean getIsElectronicAuction() {
        return isElectronicAuction;
    }

    /**
     * @param newIsElectronicAuction
     *         the isElectronicAuction to set
     *
     * @return instance of {@code T} class for chaining
     */
    public final T setIsElectronicAuction(final Boolean newIsElectronicAuction) {
        this.isElectronicAuction = newIsElectronicAuction;
        return (T) this;
    }

    /**
     * @return the cancellationDate
     */
    public final LocalDate getCancellationDate() {
        return cancellationDate;
    }

    /**
     * @param newCancellationDate
     *         the cancellationDate to set
     *
     * @return instance of {@code T} class for chaining
     */
    public final T setCancellationDate(final LocalDate newCancellationDate) {
        this.cancellationDate = newCancellationDate;
        return (T) this;
    }

    /**
     * @return the cancellationReason
     */
    public final String getCancellationReason() {
        return cancellationReason;
    }

    /**
     * @param newCancellationReason
     *         the cancellationReason to set
     *
     * @return instance of {@code T} class for chaining
     */
    public final T setCancellationReason(final String newCancellationReason) {
        this.cancellationReason = newCancellationReason;
        return (T) this;
    }

    /**
     * @return the estimatedPrice
     */
    public final Price getEstimatedPrice() {
        return estimatedPrice;
    }

    /**
     * @param newEstimatedPrice
     *         the estimatedPrice to set
     *
     * @return instance of {@code T} class for chaining
     */
    public final T setEstimatedPrice(final Price newEstimatedPrice) {
        this.estimatedPrice = newEstimatedPrice;
        return (T) this;
    }

    /**
     * @return number of envisaged candidates
     */
    public final Integer getEnvisagedCandidatesCount() {
        return envisagedCandidatesCount;
    }

    /**
     * @param newEnvisagedCandidatesCount
     *         number of envisaged candidates
     *
     * @return this instance for chaining
     */
    public final T setEnvisagedCandidatesCount(final Integer newEnvisagedCandidatesCount) {
        this.envisagedCandidatesCount = newEnvisagedCandidatesCount;
        return (T) this;
    }

    /**
     * @return minimum number of envisaged candidates
     */
    public final Integer getEnvisagedMinCandidatesCount() {
        return envisagedMinCandidatesCount;
    }

    /**
     * @param newEnvisagedMinCandidatesCount
     *         minimum number of envisaged candidates
     *
     * @return this instance for chaining
     */
    public final T setEnvisagedMinCandidatesCount(final Integer newEnvisagedMinCandidatesCount) {
        this.envisagedMinCandidatesCount = newEnvisagedMinCandidatesCount;
        return (T) this;
    }

    /**
     * @return maximum number of envisaged candidates
     */
    public final Integer getEnvisagedMaxCandidatesCount() {
        return envisagedMaxCandidatesCount;
    }

    /**
     * @param newEnvisagedMaxCandidatesCount
     *         maximum number of envisaged candidates
     *
     * @return this instance for chaining
     */
    public final T setEnvisagedMaxCandidatesCount(final Integer newEnvisagedMaxCandidatesCount) {
        this.envisagedMaxCandidatesCount = newEnvisagedMaxCandidatesCount;
        return (T) this;
    }

    /**
     * @return objective criteria for choosing the limited number of candidates
     */
    public final String getLimitedCandidatesCountCriteria() {
        return limitedCandidatesCountCriteria;
    }

    /**
     * @param newLimitedCandidatesCountCriteria
     *         objective criteria for choosing the limited number of candidates
     *
     * @return this instance for chaining
     */
    public final T setLimitedCandidatesCountCriteria(final String newLimitedCandidatesCountCriteria) {
        this.limitedCandidatesCountCriteria = newLimitedCandidatesCountCriteria;
        return (T) this;
    }
}
