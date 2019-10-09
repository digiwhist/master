package eu.dl.dataaccess.dto.generic;

import com.fasterxml.jackson.annotation.JsonIgnore;
import eu.dl.dataaccess.annotation.Transformable;
import eu.dl.dataaccess.dto.clean.Validable;
import eu.dl.dataaccess.dto.codetables.CorrectionType;
import eu.dl.dataaccess.dto.matched.MasterablePart;
import eu.dl.dataaccess.utils.ClassUtils;
import eu.dl.dataaccess.utils.ValidationUtils;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

/**
 * Corrigendum.
 */
@Transformable
public class Corrigendum implements Validable, MasterablePart {
    /**
     * Number of section that is corrected (usually with subsections separated
     * by dot, eg. III.2).
     */
    private String sectionNumber;

    /**
     * Number of contract lot which the corrigendum applies for.
     */
    private Integer lotNumber;

    /**
     * Place of text to be modified.
     */
    private String placeOfModifiedText;

    /**
     * Original information to be modified.
     */
    private String original;

    /**
     * New modified information.
     */
    private String replacement;

    /**
     * Original CPV to be modified.
     */
    private List<CPV> originalCpvs;

    /**
     * New modified CPV.
     */
    private List<CPV> replacementCpvs;

    /**
     * Original date to be modified.
     */
    private LocalDateTime originalDate;

    /**
     * New modified date.
     */
    private LocalDateTime replacementDate;

    /**
     * Original value (price).
     */
    private Price originalValue;

    /**
     * New value (price).
     */
    private Price replacementValue;

    /**
     * Publication date.
     */
    private LocalDate publicationDate;

    /**
     * Whether the correction was applied.
     */
    private Boolean isIncluded;

    /**
     * Type of the applied correction.
     */
    private CorrectionType type;

    /**
     * The order in which the correction was applied.
     */
    private Integer processingOrder;

    /**
     * @return the type.
     */
    public final CorrectionType getType() {
        return type;
    }

    /**
     * @param type - correction type to set
     * @return this instance for chaining
     */
    public final Corrigendum setType(final CorrectionType type) {
        this.type = type;
        return this;
    }

    /**
     * @return the sectionNumber
     */
    public final String getSectionNumber() {
        return sectionNumber;
    }

    /**
     * @param sectionNumber
     *            the sectionNumber to set
     * @return this instance for chaining
     */
    public final Corrigendum setSectionNumber(final String sectionNumber) {
        this.sectionNumber = sectionNumber;
        return this;
    }

    /**
     * @return the lotNumber
     */
    public final Integer getLotNumber() {
        return lotNumber;
    }

    /**
     * @param lotNumber
     *            the lotNumber to set
     * @return this instance for chaining
     */
    public final Corrigendum setLotNumber(final Integer lotNumber) {
        this.lotNumber = lotNumber;
        return this;
    }

    /**
     * @return the placeOfModifiedText
     */
    public final String getPlaceOfModifiedText() {
        return placeOfModifiedText;
    }

    /**
     * @param placeOfModifiedText
     *            the placeOfModifiedText to set
     * @return this instance for chaining
     */
    public final Corrigendum setPlaceOfModifiedText(final String placeOfModifiedText) {
        this.placeOfModifiedText = placeOfModifiedText;
        return this;
    }

    /**
     * @return the original
     */
    public final String getOriginal() {
        return original;
    }

    /**
     * @param original
     *            the original to set
     * @return this instance for chaining
     */
    public final Corrigendum setOriginal(final String original) {
        this.original = original;
        return this;
    }

    /**
     * @return the replacement
     */
    public final String getReplacement() {
        return replacement;
    }

    /**
     * @param replacement
     *            the replacement to set
     * @return this instance for chaining
     */
    public final Corrigendum setReplacement(final String replacement) {
        this.replacement = replacement;
        return this;
    }

    /**
     * @return the originalCpvs
     */
    public final List<CPV> getOriginalCpvs() {
        return originalCpvs;
    }

    /**
     * @param originalCpvs
     *            the originalCpvs to set
     * @return this instance for chaining
     */
    public final Corrigendum setOriginalCpvs(final List<CPV> originalCpvs) {
        this.originalCpvs = originalCpvs;
        return this;
    }

    /**
     * @return the replacementCpvs
     */
    public final List<CPV> getReplacementCpvs() {
        return replacementCpvs;
    }

    /**
     * @param replacementCpvs
     *            the replacementCpvs to set
     * @return this instance for chaining
     */
    public final Corrigendum setReplacementCpvs(final List<CPV> replacementCpvs) {
        this.replacementCpvs = replacementCpvs;
        return this;
    }

    /**
     * @return the originalDate
     */
    public final LocalDateTime getOriginalDate() {
        return originalDate;
    }

    /**
     * @param originalDate
     *            the originalDate to set
     * @return this instance for chaining
     */
    public final Corrigendum setOriginalDate(final LocalDateTime originalDate) {
        this.originalDate = originalDate;
        return this;
    }

    /**
     * @return the replacementDate
     */
    public final LocalDateTime getReplacementDate() {
        return replacementDate;
    }

    /**
     * @param replacementDate
     *            the replacementDate to set
     * @return this instance for chaining
     */
    public final Corrigendum setReplacementDate(final LocalDateTime replacementDate) {
        this.replacementDate = replacementDate;
        return this;
    }

    /**
     * Gets originalValue.
     *
     * @return value of originalValue
     */
    public final Price getOriginalValue() {
        return originalValue;
    }

    /**
     * Sets originalValue.
     *
     * @param originalValue
     *         the originalValue to set
     *
     * @return this instance for chaining
     */
    public final Corrigendum setOriginalValue(final Price originalValue) {
        this.originalValue = originalValue;
        return this;
    }

    /**
     * Gets replacementValue.
     *
     * @return value of replacementValue
     */
    public final Price getReplacementValue() {
        return replacementValue;
    }

    /**
     * Sets replacementValue.
     *
     * @param replacementValue
     *         the replacementValue to set
     *
     * @return this instance for chaining
     */
    public final Corrigendum setReplacementValue(final Price replacementValue) {
        this.replacementValue = replacementValue;
        return this;
    }

    @Override
    @JsonIgnore
    public final Corrigendum getValid() {
        setOriginalCpvs(ValidationUtils.getValid(originalCpvs));
        setReplacementCpvs(ValidationUtils.getValid(replacementCpvs));
        setOriginalValue(ClassUtils.removeNonsenses(originalValue));
        setReplacementValue(ClassUtils.removeNonsenses(replacementValue));

        return ValidationUtils.getValid(this, lotNumber, original, originalCpvs, originalDate, originalValue,
            placeOfModifiedText, replacement, replacementCpvs, replacementDate, replacementValue, sectionNumber);
    }

    @Override
    public final String getTenderId() {
        return null;
    }

    @Override
    public final LocalDate getPublicationDate() {
        return publicationDate;
    }

    @Override
    public final LocalDateTime getCreatedRaw() {
        return null;
    }

    @Override
    public final Corrigendum setPublicationDate(final LocalDate date) {
        this.publicationDate = date;
        return this;
    }

    /**
     * @return TRUE if the correction was applied, otherwise FALSE
     */
    public final Boolean getIsIncluded() {
        return isIncluded;
    }

    /**
     * @param included
     *      whether the correction was applied
     * @return this instance for chaining
     */
    public final Corrigendum setIsIncluded(final Boolean included) {
        isIncluded = included;
        return this;
    }

    /**
     * @return processing order of the correction
     */
    public final Integer getProcessingOrder() {
        return processingOrder;
    }

    /**
     * @param processingOrder
     *      processing order to be set
     * @return this instance for chaining
     */
    public final Corrigendum setProcessingOrder(final Integer processingOrder) {
        this.processingOrder = processingOrder;
        return this;
    }
}
