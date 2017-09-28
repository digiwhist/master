package eu.dl.dataaccess.dto.parsed;

// TODO: Auto-generated Javadoc

import java.util.ArrayList;
import java.util.List;

/**
 * Corrigendum.
 */
public class ParsedCorrigendum {
    /**
     * Number of section that is corrected.
     */
    private String sectionNumber;

    /**
     * Number of contract lot which the corrigendum applies for.
     */
    private String lotNumber;

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
    private List<ParsedCPV> originalCpvs;

    /**
     * New modified CPV.
     */
    private List<ParsedCPV> replacementCpvs;

    /**
     * Original date to be modified.
     */
    private String originalDate;

    /**
     * New modified date.
     */
    private String replacementDate;

    /**
     * Original value (price).
     */
    private ParsedPrice originalValue;

    /**
     * New value (price).
     */
    private ParsedPrice replacementValue;

    /**
     * Gets the section number.
     *
     * @return the section number
     */
    public final String getSectionNumber() {
        return sectionNumber;
    }

    /**
     * Sets the section number.
     *
     * @param sectionNumber
     *         the section number
     *
     * @return the parsed corrigendum
     */
    public final ParsedCorrigendum setSectionNumber(final String sectionNumber) {
        this.sectionNumber = sectionNumber;
        return this;
    }

    /**
     * Gets the lot number.
     *
     * @return the lot number
     */
    public final String getLotNumber() {
        return lotNumber;
    }

    /**
     * Sets the lot number.
     *
     * @param lotNumber
     *         the lot number
     *
     * @return the parsed corrigendum
     */
    public final ParsedCorrigendum setLotNumber(final String lotNumber) {
        this.lotNumber = lotNumber;
        return this;
    }

    /**
     * Gets the place of modified text.
     *
     * @return the place of modified text
     */
    public final String getPlaceOfModifiedText() {
        return placeOfModifiedText;
    }

    /**
     * Sets the place of modified text.
     *
     * @param placeOfModifiedText
     *         the place of modified text
     *
     * @return the parsed corrigendum
     */
    public final ParsedCorrigendum setPlaceOfModifiedText(final String placeOfModifiedText) {
        this.placeOfModifiedText = placeOfModifiedText;
        return this;
    }

    /**
     * Gets the original.
     *
     * @return the original
     */
    public final String getOriginal() {
        return original;
    }

    /**
     * Sets the original.
     *
     * @param original
     *         the original
     *
     * @return the parsed corrigendum
     */
    public final ParsedCorrigendum setOriginal(final String original) {
        this.original = original;
        return this;
    }

    /**
     * Gets the replacement.
     *
     * @return the replacement
     */
    public final String getReplacement() {
        return replacement;
    }

    /**
     * Sets the replacement.
     *
     * @param replacement
     *         the replacement
     *
     * @return the parsed corrigendum
     */
    public final ParsedCorrigendum setReplacement(final String replacement) {
        this.replacement = replacement;
        return this;
    }

    /**
     * Gets the original cpv.
     *
     * @return the original cpvs
     */
    public final List<ParsedCPV> getOriginalCpvs() {
        return originalCpvs;
    }

    /**
     * Sets the original cpv.
     *
     * @param originalCpvs
     *         the original cpvs
     *
     * @return the parsed corrigendum
     */
    public final ParsedCorrigendum setOriginalCpvs(final List<ParsedCPV> originalCpvs) {
        this.originalCpvs = originalCpvs;
        return this;
    }

    /**
     * Gets the replacement cpv.
     *
     * @return the replacement cpvs
     */
    public final List<ParsedCPV> getReplacementCpvs() {
        return replacementCpvs;
    }

    /**
     * Sets the replacement cpv.
     *
     * @param replacementCpvs
     *         the replacement cpvs
     *
     * @return the parsed corrigendum
     */
    public final ParsedCorrigendum setReplacementCpvs(final List<ParsedCPV> replacementCpvs) {
        this.replacementCpvs = replacementCpvs;
        return this;
    }

    /**
     * Gets the original date.
     *
     * @return the original date
     */
    public final String getOriginalDate() {
        return originalDate;
    }

    /**
     * Sets the original date.
     *
     * @param originalDate
     *         the original date
     *
     * @return the parsed corrigendum
     */
    public final ParsedCorrigendum setOriginalDate(final String originalDate) {
        this.originalDate = originalDate;
        return this;
    }

    /**
     * Gets the replacement date.
     *
     * @return the replacement date
     */
    public final String getReplacementDate() {
        return replacementDate;
    }

    /**
     * Sets the replacement date.
     *
     * @param replacementDate
     *         the replacement date
     *
     * @return the parsed corrigendum
     */
    public final ParsedCorrigendum setReplacementDate(final String replacementDate) {
        this.replacementDate = replacementDate;
        return this;
    }

    /**
     * Gets originalValue.
     *
     * @return value of originalValue
     */
    public final ParsedPrice getOriginalValue() {
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
    public final ParsedCorrigendum setOriginalValue(final ParsedPrice originalValue) {
        this.originalValue = originalValue;
        return this;
    }

    /**
     * Gets replacementValue.
     *
     * @return value of replacementValue
     */
    public final ParsedPrice getReplacementValue() {
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
    public final ParsedCorrigendum setReplacementValue(final ParsedPrice replacementValue) {
        this.replacementValue = replacementValue;
        return this;
    }

    /**
     * Adds the original CPV.
     *
     * @param originalCpv
     *         the original cpv
     *
     * @return the parsed corrigendum
     */
    public final ParsedCorrigendum addOriginalCpv(final ParsedCPV originalCpv) {
        if (originalCpv != null) {
            if (getOriginalCpvs() == null) {
                setOriginalCpvs(new ArrayList<>());
            }

            this.originalCpvs.add(originalCpv);
        }
        return this;
    }

    /**
     * Adds the replacement CPV.
     *
     * @param replacementCpv
     *         the replacement cpv
     *
     * @return the parsed corrigendum
     */
    public final ParsedCorrigendum addReplacementCpv(final ParsedCPV replacementCpv) {
        if (replacementCpv != null) {
            if (getReplacementCpvs() == null) {
                setReplacementCpvs(new ArrayList<>());
            }

            this.replacementCpvs.add(replacementCpv);
        }
        return this;
    }
}
