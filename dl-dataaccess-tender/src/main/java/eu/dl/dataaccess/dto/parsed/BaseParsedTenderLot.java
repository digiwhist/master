package eu.dl.dataaccess.dto.parsed;

import java.util.ArrayList;
import java.util.List;

/**
 * Abstract parsed tender lot DTO containing properties that may also be
 * directly within the tender level.
 *
 * @param <T> extending class (for fluent interface purposes)
 */
public abstract class BaseParsedTenderLot<T> extends BaseParsedStorableDTO {
    /**
     * Tender/Lot title.
     */
    private String title;

    /**
     * Tender/Lot title in English.
     */
    private String titleEnglish;

    /**
     * Subject description.
     */
    private String description;

    /**
     * Subject description in English.
     */
    private String descriptionEnglish;

    /**
     * Tender/Lot is awarded.
     */
    private String isAwarded;

    /**
     * Acceptance of variant offers.
     */
    private String areVariantsAccepted;

    /**
     * Lot has options.
     */
    private String hasOptions;

    /**
     * Describes required qualification of bidder.
     */
    private String eligibilityCriteria;

    /**
     * Tender/Lot is covered by GPA.
     */
    private String isCoveredByGpa;

    /**
     * Tender/Lot is awarded as superior framework agreement.
     */
    private String isFrameworkAgreement;

    /**
     * Envisaged maximum number of participants to the framework agreement.
     */
    private String maxFrameworkAgreementParticipants;

    /**
     * The exact address of the Tender/Lot performance.
     */
    private ParsedAddress addressOfImplementation;

    /**
     * Is either initiation of dynamic purchasing system, or a purchase via one.
     */
    private String isDps;

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
     * Date of decision on Tender/Lot award.
     */
    private String awardDecisionDate;

    /**
     * Date of contract signature.
     */
    private String contractSignatureDate;

    /**
     * Cpv codes of the subject.
     */
    private List<ParsedCPV> cpvs;

    /**
     * List of Tender/Lot fundings.
     */
    private List<ParsedFunding> fundings;

    /**
     * Method used for bids evaluation (lowest price or MEAT).
     */
    private String selectionMethod;

    /**
     * List of awarding criteria.
     */
    private List<ParsedAwardCriterion> awardCriteria;

    /**
     * Is awarded by electronic auction.
     */
    private String isElectronicAuction;

    /**
     * Date of Tender/Lot cancellation (or its publication).
     */
    private String cancellationDate;

    /**
     * Stated reason for cancellation.
     */
    private String cancellationReason;

    /**
     * Estimated value of Tender/Lot.
     */
    private ParsedPrice estimatedPrice;

    /**
     * Number of envisaged candidates.
     */
    private String envisagedCandidatesCount;

    /**
     * Min number of envisaged candidates.
     */
    private String envisagedMinCandidatesCount;

    /**
     * Max number of envisaged candidates.
     */
    private String envisagedMaxCandidatesCount;

    /**
     * Objective criteria for choosing the limited number of candidates.
     */
    private String limitedCandidatesCountCriteria;

    /**
     * @return the title
     */
    public final String getTitle() {
        return title;
    }

    /**
     * @param newTitle the title to set
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
     * @param newTitleEnglish the titleEnglish to set
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
     * @param newDescription the description to set
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
     * @param newDescriptionEnglish the descriptionEnglish to set
     * @return instance of {@code T} class for chaining
     */
    public final T setDescriptionEnglish(final String newDescriptionEnglish) {
        this.descriptionEnglish = newDescriptionEnglish;
        return (T) this;
    }

    /**
     * @return whether tender/lot is awarded
     */
    public final String getIsAwarded() {
        return isAwarded;
    }

    /**
     * @param newIsAwarded whether tender/lot is awarded
     * @return this instance for chaining
     */
    public final T setIsAwarded(final String newIsAwarded) {
        this.isAwarded = newIsAwarded;
        return (T) this;
    }

    /**
     * @return true if variant offers are accepted
     */
    public final String getAreVariantsAccepted() {
        return areVariantsAccepted;
    }

    /**
     * @param newAreVariantsAccepted whether variant offers are accepted
     * @return this instance for chaining
     */
    public final T setAreVariantsAccepted(final String newAreVariantsAccepted) {
        this.areVariantsAccepted = newAreVariantsAccepted;
        return (T) this;
    }

    /**
     * @return whether lot has options
     */
    public final String getHasOptions() {
        return hasOptions;
    }

    /**
     * @param newHasOptions whether lot has options
     * @return this instance for chaining
     */
    public final T setHasOptions(final String newHasOptions) {
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
     * @param newEligibilityCriteria the eligibilityCriteria to set
     * @return this instance for for chaining
     */
    public final T setEligibilityCriteria(final String newEligibilityCriteria) {
        this.eligibilityCriteria = newEligibilityCriteria;
        return (T) this;
    }

    /**
     * @return the isCoveredByGpa
     */
    public final String getIsCoveredByGpa() {
        return isCoveredByGpa;
    }

    /**
     * @param newIsCoveredByGpa the isCoveredByGpa to set
     * @return instance of {@code T} class for chaining
     */
    public final T setIsCoveredByGpa(final String newIsCoveredByGpa) {
        this.isCoveredByGpa = newIsCoveredByGpa;
        return (T) this;
    }

    /**
     * @return the isFrameworkAgreement
     */
    public final String getIsFrameworkAgreement() {
        return isFrameworkAgreement;
    }

    /**
     * @param newIsFrameworkAgreement the isFrameworkAgreement to set
     * @return instance of {@code T} class for chaining
     */
    public final T setIsFrameworkAgreement(final String newIsFrameworkAgreement) {
        this.isFrameworkAgreement = newIsFrameworkAgreement;
        return (T) this;
    }

    /**
     * @return the maxFrameworkAgreementParticipants
     */
    public final String getMaxFrameworkAgreementParticipants() {
        return maxFrameworkAgreementParticipants;
    }

    /**
     * @param newMaxFrameworkAgreementParticipants the maxFrameworkAgreementParticipants to set
     * @return instance of {@code T} class for chaining
     */
    public final T setMaxFrameworkAgreementParticipants(final String newMaxFrameworkAgreementParticipants) {
        this.maxFrameworkAgreementParticipants = newMaxFrameworkAgreementParticipants;
        return (T) this;
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
    public final T setAddressOfImplementation(final ParsedAddress newAddressOfImplementation) {
        this.addressOfImplementation = newAddressOfImplementation;
        return (T) this;
    }

    /**
     * @return the isDps
     */
    public final String getIsDps() {
        return isDps;
    }

    /**
     * @param newIsDps the isDps to set
     * @return instance of {@code T} class for chaining
     */
    public final T setIsDps(final String newIsDps) {
        this.isDps = newIsDps;
        return (T) this;
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
    public final T setEstimatedStartDate(final String newEstimatedStartDate) {
        this.estimatedStartDate = newEstimatedStartDate;
        return (T) this;
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
    public final T setEstimatedCompletionDate(final String newEstimatedCompletionDate) {
        this.estimatedCompletionDate = newEstimatedCompletionDate;
        return (T) this;
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
    public final T setEstimatedDurationInYears(final String newEstimatedDurationInYears) {
        this.estimatedDurationInYears = newEstimatedDurationInYears;
        return (T) this;
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
    public final T setEstimatedDurationInMonths(final String newEstimatedDurationInMonths) {
        this.estimatedDurationInMonths = newEstimatedDurationInMonths;
        return (T) this;
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
    public final T setEstimatedDurationInDays(final String newEstimatedDurationInDays) {
        this.estimatedDurationInDays = newEstimatedDurationInDays;
        return (T) this;
    }

    /**
     * @return the awardDecisionDate
     */
    public final String getAwardDecisionDate() {
        return awardDecisionDate;
    }

    /**
     * @param newAwardDecisionDate the awardDecisionDate to set
     * @return instance of {@code T} class for chaining
     */
    public final T setAwardDecisionDate(final String newAwardDecisionDate) {
        this.awardDecisionDate = newAwardDecisionDate;
        return (T) this;
    }

    /**
     * @return the contractSignatureDate
     */
    public final String getContractSignatureDate() {
        return contractSignatureDate;
    }

    /**
     * @param newContractSignatureDate the contractSignatureDate to set
     * @return instance of {@code T} class for chaining
     */
    public final T setContractSignatureDate(final String newContractSignatureDate) {
        this.contractSignatureDate = newContractSignatureDate;
        return (T) this;
    }

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
    public final T setCpvs(final List<ParsedCPV> newCpvs) {
        this.cpvs = newCpvs;
        return (T) this;
    }

    /**
     * @return the fundings
     */
    public final List<ParsedFunding> getFundings() {
        return fundings;
    }

    /**
     * @param newFundings the fundings to set
     * @return instance of {@code T} class for chaining
     */
    public final T setFundings(final List<ParsedFunding> newFundings) {
        this.fundings = newFundings;
        return (T) this;
    }

    /**
     * @return the selection method
     */
    public final String getSelectionMethod() {
        return selectionMethod;
    }

    /**
     * @param newSelectionMethod the selection method to set
     * @return instance of {@code T} class for chaining
     */
    public final T setSelectionMethod(final String newSelectionMethod) {
        this.selectionMethod = newSelectionMethod;
        return (T) this;
    }

    /**
     * @return the awardCriteria
     */
    public final List<ParsedAwardCriterion> getAwardCriteria() {
        return awardCriteria;
    }

    /**
     * @param newAwardCriteria the awardCriteria to set
     * @return instance of {@code T} class for chaining
     */
    public final T setAwardCriteria(final List<ParsedAwardCriterion> newAwardCriteria) {
        this.awardCriteria = newAwardCriteria;
        return (T) this;
    }

    /**
     * @return the isElectronicAuction
     */
    public final String getIsElectronicAuction() {
        return isElectronicAuction;
    }

    /**
     * @param newIsElectronicAuction the isElectronicAuction to set
     * @return instance of {@code T} class for chaining
     */
    public final T setIsElectronicAuction(final String newIsElectronicAuction) {
        this.isElectronicAuction = newIsElectronicAuction;
        return (T) this;
    }

    /**
     * @return the cancellationDate
     */
    public final String getCancellationDate() {
        return cancellationDate;
    }

    /**
     * @param newCancellationDate the cancellationDate to set
     * @return instance of {@code T} class for chaining
     */
    public final T setCancellationDate(final String newCancellationDate) {
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
     * @param newCancellationReason the cancellationReason to set
     * @return instance of {@code T} class for chaining
     */
    public final T setCancellationReason(final String newCancellationReason) {
        this.cancellationReason = newCancellationReason;
        return (T) this;
    }

    /**
     * @return the estimatedPrice
     */
    public final ParsedPrice getEstimatedPrice() {
        return estimatedPrice;
    }

    /**
     * @param newEstimatedPrice the estimatedPrice to set
     * @return instance of {@code T} class for chaining
     */
    public final T setEstimatedPrice(final ParsedPrice newEstimatedPrice) {
        this.estimatedPrice = newEstimatedPrice;
        return (T) this;
    }

    /**
     * @return number of envisaged candidates
     */
    public final String getEnvisagedCandidatesCount() {
        return envisagedCandidatesCount;
    }

    /**
     * @param newEnvisagedCandidatesCount number of envisaged candidates
     * @return this instance for chaining
     */
    public final T setEnvisagedCandidatesCount(final String newEnvisagedCandidatesCount) {
        this.envisagedCandidatesCount = newEnvisagedCandidatesCount;
        return (T) this;
    }

    /**
     * @return minimum number of envisaged candidates
     */
    public final String getEnvisagedMinCandidatesCount() {
        return envisagedMinCandidatesCount;
    }

    /**
     * @param newEnvisagedMinCandidatesCount minimum number of envisaged candidates
     * @return this instance for chaining
     */
    public final T setEnvisagedMinCandidatesCount(final String newEnvisagedMinCandidatesCount) {
        this.envisagedMinCandidatesCount = newEnvisagedMinCandidatesCount;
        return (T) this;
    }

    /**
     * @return maximum number of envisaged candidates
     */
    public final String getEnvisagedMaxCandidatesCount() {
        return envisagedMaxCandidatesCount;
    }

    /**
     * @param newEnvisagedMaxCandidatesCount maximum number of envisaged candidates
     * @return this instance for chaining
     */
    public final T setEnvisagedMaxCandidatesCount(final String newEnvisagedMaxCandidatesCount) {
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
     * @param newLimitedCandidatesCountCriteria objective criteria for choosing the limited number of candidates
     * @return this instance for chaining
     */
    public final T setLimitedCandidatesCountCriteria(final String newLimitedCandidatesCountCriteria) {
        this.limitedCandidatesCountCriteria = newLimitedCandidatesCountCriteria;
        return (T) this;
    }

    /**
     * Adds criterion to the list of award criteria or create a new list with given criterion if none exists.
     *
     * @param criterion new award criterion to be added
     * @return instance of {@code T} class for chaining
     */
    public final T addAwardCriterion(final ParsedAwardCriterion criterion) {
        if (criterion != null) {
            if (getAwardCriteria() == null) {
                setAwardCriteria(new ArrayList<>());
            }
            this.awardCriteria.add(criterion);
        }

        return (T) this;
    }

    /**
     * Adds funding to the list of fundings or create a new list with given funding if none exists.
     *
     * @param funding new funding to be added
     * @return instance of {@code T} class for chaining
     */
    public final T addFunding(final ParsedFunding funding) {
        if (funding != null) {
            if (getFundings() == null) {
                setFundings(new ArrayList<>());
            }
            this.fundings.add(funding);
        }
        return (T) this;
    }

    /**
     * Adds CPV to the list of CPVs or create a new list with given CPV if none exists.
     *
     * @param cpv new CPV to be added
     * @return instance of {@code T} class for chaining
     */
    public final T addCpv(final ParsedCPV cpv) {
        if (cpv != null) {
            if (getCpvs() == null) {
                setCpvs(new ArrayList<>());
            }
            this.cpvs.add(cpv);
        }
        return (T) this;
    }

    /**
     * Adds award criteria to the list of award criteria or create a new list with given award criteria if none exists.
     *
     * @param newAwardCriteria awardCriteria
     *                       new award criteria to be added
     * @return instance of {@code T} class for chaining
     */
    public final T addAwardCriteria(final List<ParsedAwardCriterion> newAwardCriteria) {
        if (newAwardCriteria != null && !newAwardCriteria.isEmpty()) {
            if (getAwardCriteria() == null) {
                setAwardCriteria(new ArrayList<>());
            }

            this.awardCriteria.addAll(newAwardCriteria);
        }

        return (T) this;
    }
}

