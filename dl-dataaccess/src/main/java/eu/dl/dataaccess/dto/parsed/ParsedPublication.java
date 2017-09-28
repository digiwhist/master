package eu.dl.dataaccess.dto.parsed;

// TODO: Auto-generated Javadoc

import com.fasterxml.jackson.annotation.JsonTypeInfo;

/**
 * Publication. Describes info on contract/notice published on one individual
 * web page or other source.
 */
@JsonTypeInfo(use=JsonTypeInfo.Id.CLASS, include=JsonTypeInfo.As.PROPERTY, property="@class",
    defaultImpl=ParsedPublication.class)
public class ParsedPublication {

    /**
     * URL of source (e.g. www.ted.europa.eu,
     * http://reznictvi-dvorak.profilzadavatele.cz/, ...).
     */
    private String source;

    /**
     * Identifier of publication on source.
     */
    private String sourceId;

    /**
     * Identifier of contract on source, ideally this one should be referred by
     * assignedContractId.
     */
    private String sourceTenderId;

    /**
     * File reference number attributed by the contracting authority.
     */
    private String buyerAssignedId;

    /**
     * URL of contract on source, where machine readable data are present.
     */
    private String machineReadableUrl;

    /**
     * URL of contract on source, where human readable data are present.
     */
    private String humanReadableUrl;

    /**
     * Date of publication (of given version).
     */
    private String publicationDate;

    /**
     * Date of dispatch (different from publication date).
     */
    private String dispatchDate;

    /**
     * Language code of the publication.
     */
    private String language;

    /**
     * Version number of publication on given source.
     */
    private String version;

    /**
     * Is the publication valid or made obsolete by another listed publication
     * (so it is sufficient to look into other publications).
     */
    private String isValid;

    /**
     * Referred publication data is already merged into this publication
     * instance.
     */
    private String isIncluded;

    /**
     * Information on whether the announcement was obligatory to be published.
     */
    private String isMandatory;

    /**
     * Identifies given type of publication.
     */
    private String formType;

    /**
     * Source specific type of form.
     */
    private String sourceFormType;

    /**
     * Date of last change on source (should there be any changes in already
     * published documents).
     */
    private String lastUpdate;

    /**
     * Is publication related to parent contract? (framework agreement, DPS,
     * ...).
     */
    private String isParentTender;

    /**
     * Gets the source.
     *
     * @return the source
     */

    public final String getSource() {
        return source;
    }

    /**
     * Sets the source.
     *
     * @param source
     *            the source
     * @return the parsed publication
     */

    public final ParsedPublication setSource(final String source) {
        this.source = source;
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
     * @return the parsed publication
     */

    public final ParsedPublication setSourceId(final String sourceId) {
        this.sourceId = sourceId;
        return this;
    }

    /**
     * Gets the source tender id.
     *
     * @return the source tender id
     */

    public final String getSourceTenderId() {
        return sourceTenderId;
    }

    /**
     * Sets the source tender id.
     *
     * @param sourceTenderId
     *            the source tender id
     * @return the parsed publication
     */

    public final ParsedPublication setSourceTenderId(final String sourceTenderId) {
        this.sourceTenderId = sourceTenderId;
        return this;
    }

    /**
     * Gets the buyer assigned id.
     *
     * @return the buyer assigned id
     */

    public final String getBuyerAssignedId() {
        return buyerAssignedId;
    }

    /**
     * Sets the buyer assigned id.
     *
     * @param buyerAssignedId
     *            the buyer assigned id
     * @return the parsed publication
     */

    public final ParsedPublication setBuyerAssignedId(final String buyerAssignedId) {
        this.buyerAssignedId = buyerAssignedId;
        return this;
    }

    /**
     * Gets the machine readable url.
     *
     * @return the machine readable url
     */

    public final String getMachineReadableUrl() {
        return machineReadableUrl;
    }

    /**
     * Sets the machine readable url.
     *
     * @param machineReadableUrl
     *            the machine readable url
     * @return the parsed publication
     */

    public final ParsedPublication setMachineReadableUrl(final String machineReadableUrl) {
        this.machineReadableUrl = machineReadableUrl;
        return this;
    }

    /**
     * Gets the human readable url.
     *
     * @return the human readable url
     */

    public final String getHumanReadableUrl() {
        return humanReadableUrl;
    }

    /**
     * Sets the human readable url.
     *
     * @param humanReadableUrl
     *            the human readable url
     * @return the parsed publication
     */

    public final ParsedPublication setHumanReadableUrl(final String humanReadableUrl) {
        this.humanReadableUrl = humanReadableUrl;
        return this;
    }

    /**
     * Gets the publication date.
     *
     * @return the publication date
     */

    public final String getPublicationDate() {
        return publicationDate;
    }

    /**
     * Sets the publication date.
     *
     * @param publicationDate
     *            the publication date
     * @return the parsed publication
     */

    public final ParsedPublication setPublicationDate(final String publicationDate) {
        this.publicationDate = publicationDate;
        return this;
    }

    /**
     * Gets the dispatch date.
     *
     * @return the dispatch date
     */

    public final String getDispatchDate() {
        return dispatchDate;
    }

    /**
     * Sets the dispatch date.
     *
     * @param dispatchDate
     *            the dispatch date
     * @return the parsed publication
     */

    public final ParsedPublication setDispatchDate(final String dispatchDate) {
        this.dispatchDate = dispatchDate;
        return this;
    }

    /**
     * Gets the language.
     *
     * @return the language
     */

    public final String getLanguage() {
        return language;
    }

    /**
     * Sets the language.
     *
     * @param language
     *            the language
     * @return the parsed publication
     */

    public final ParsedPublication setLanguage(final String language) {
        this.language = language;
        return this;
    }

    /**
     * Gets the version.
     *
     * @return the version
     */

    public final String getVersion() {
        return version;
    }

    /**
     * Sets the version.
     *
     * @param version
     *            the version
     * @return the parsed publication
     */

    public final ParsedPublication setVersion(final String version) {
        this.version = version;
        return this;
    }

    /**
     * Gets the checks if is valid.
     *
     * @return the checks if is valid
     */

    public final String getIsValid() {
        return isValid;
    }

    /**
     * Sets the is valid.
     *
     * @param isValid
     *            the is valid
     * @return the parsed publication
     */

    public final ParsedPublication setIsValid(final boolean isValid) {
        this.isValid = Boolean.toString(isValid);
        return this;
    }

    /**
     * Gets the checks if is included.
     *
     * @return the checks if is included
     */

    public final String getIsIncluded() {
        return isIncluded;
    }

    /**
     * Sets the is included.
     *
     * @param isIncluded
     *            the is included
     * @return the parsed publication
     */

    public final ParsedPublication setIsIncluded(final boolean isIncluded) {
        this.isIncluded = Boolean.toString(isIncluded);
        return this;
    }

    /**
     * Gets the checks if is mandatory.
     *
     * @return the checks if is mandatory
     */

    public final String getIsMandatory() {
        return isMandatory;
    }

    /**
     * Sets the is mandatory.
     *
     * @param isMandatory
     *            the is mandatory
     * @return the parsed publication
     */

    public final ParsedPublication setIsMandatory(final String isMandatory) {
        this.isMandatory = isMandatory;
        return this;
    }

    /**
     * Gets the form type.
     *
     * @return the form type
     */

    public final String getFormType() {
        return formType;
    }

    /**
     * Sets the form type.
     *
     * @param formType
     *            the form type
     * @return the parsed publication
     */

    public final ParsedPublication setFormType(final String formType) {
        this.formType = formType;
        return this;
    }

    /**
     * Gets the source form type.
     *
     * @return the source form type
     */

    public final String getSourceFormType() {
        return sourceFormType;
    }

    /**
     * Sets the source form type.
     *
     * @param sourceFormType
     *            the source form type
     * @return the parsed publication
     */

    public final ParsedPublication setSourceFormType(final String sourceFormType) {
        this.sourceFormType = sourceFormType;
        return this;
    }

    /**
     * Gets the last update.
     *
     * @return the last update
     */

    public final String getLastUpdate() {
        return lastUpdate;
    }

    /**
     * Sets the last update.
     *
     * @param lastUpdate
     *            the last update
     * @return the parsed publication
     */

    public final ParsedPublication setLastUpdate(final String lastUpdate) {
        this.lastUpdate = lastUpdate;
        return this;
    }

    /**
     * Gets the checks if is parent tender.
     *
     * @return the checks if is parent tender
     */

    public final String getIsParentTender() {
        return isParentTender;
    }

    /**
     * Sets the is parent tender.
     *
     * @param isParentTender
     *            the is parent tender
     * @return the parsed publication
     */

    public final ParsedPublication setIsParentTender(final boolean isParentTender) {
        this.isParentTender = Boolean.toString(isParentTender);
        return this;
    }
}
