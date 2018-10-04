package eu.dl.dataaccess.dto.generic;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import eu.dl.dataaccess.annotation.Transformable;
import eu.dl.dataaccess.dto.clean.Validable;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.utils.ValidationUtils;

import java.net.URL;
import java.time.LocalDate;

/**
 * Publication. Describes info on contract/notice published on one individual
 * web page or other source.
 */
@JsonTypeInfo(use=JsonTypeInfo.Id.CLASS, include=JsonTypeInfo.As.PROPERTY, property="@class",
    defaultImpl=Publication.class)
@Transformable
public class Publication implements Validable {

    /**
     * Default constructor.
     */
    public Publication() {
    }

    /**
     * Constructor which creates copy of input publication. It is useful when we create descendant from the publication.
     *
     * @param publication
     *          publication
     */
    protected Publication(final Publication publication) {
        setSource(publication.getSource());
        setSourceId(publication.getSourceId());
        setSourceTenderId(publication.getSourceTenderId());
        setBuyerAssignedId(publication.getBuyerAssignedId());
        setMachineReadableUrl(publication.getMachineReadableUrl());
        setHumanReadableUrl(publication.getHumanReadableUrl());
        setPublicationDate(publication.getPublicationDate());
        setDispatchDate(publication.getDispatchDate());
        setLanguage(publication.getLanguage());
        setVersion(publication.getVersion());
        setIsValid(publication.getIsValid());
        setIsIncluded(publication.getIsIncluded());
        setIsMandatory(publication.getIsMandatory());
        setFormType(publication.getFormType());
        setSourceFormType(publication.getSourceFormType());
        setLastUpdate(publication.getLastUpdate());
        setIsParentTender(publication.getIsParentTender());
    }

    /**
     * URL of source (e.g. www.ted.europa.eu,
     * http://reznictvi-dvorak.profilzadavatele.cz/, ...).
     */
    private URL source;

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
    private URL machineReadableUrl;

    /**
     * URL of tender on source, where human readable data are present.
     */
    private URL humanReadableUrl;

    /**
     * Date of publication (of given version).
     */
    private LocalDate publicationDate;

    /**
     * Date of dispatch (different from publication date).
     */
    private LocalDate dispatchDate;

    /**
     * Language ISO 639 code of the publication.
     */
    private String language;

    /**
     * Version number of publication on given source.
     */
    private Integer version;

    /**
     * Is the publication valid or made obsolete by another listed publication
     * (so it is sufficient to look into other publications).
     */
    private Boolean isValid;

    /**
     * Referred publication data is already merged into this publication
     * instance.
     */
    private Boolean isIncluded;

    /**
     * Information on whether the announcement was obligatory to be published.
     */
    private Boolean isMandatory;

    /**
     * Identifies given type of publication.
     */
    private PublicationFormType formType;

    /**
     * Source specific type of form.
     */
    private String sourceFormType;

    /**
     * Date of last change on source (should there be any changes in already
     * published documents).
     */
    private LocalDate lastUpdate;

    /**
     * Is publication related to parent tender? (framework agreement, DPS, ...).
     */
    private Boolean isParentTender;

    /**
     * @return the source
     */
    public final URL getSource() {
        return source;
    }

    /**
     * @param source
     *         the source to set
     *
     * @return this instance for chaining
     */
    public final Publication setSource(final URL source) {
        this.source = source;
        return this;
    }

    /**
     * @return the sourceId
     */
    public final String getSourceId() {
        return sourceId;
    }

    /**
     * @param sourceId
     *         the sourceId to set
     *
     * @return this instance for chaining
     */
    public final Publication setSourceId(final String sourceId) {
        this.sourceId = sourceId;
        return this;
    }

    /**
     * @return the sourceTenderId
     */
    public final String getSourceTenderId() {
        return sourceTenderId;
    }

    /**
     * @param sourceTenderId
     *         the sourceTenderId to set
     *
     * @return this instance for chaining
     */
    public final Publication setSourceTenderId(final String sourceTenderId) {
        this.sourceTenderId = sourceTenderId;
        return this;
    }

    /**
     * @return the buyerAssignedId
     */
    public final String getBuyerAssignedId() {
        return buyerAssignedId;
    }

    /**
     * @param buyerAssignedId
     *         the buyerAssignedId to set
     *
     * @return this instance for chaining
     */
    public final Publication setBuyerAssignedId(final String buyerAssignedId) {
        this.buyerAssignedId = buyerAssignedId;
        return this;
    }

    /**
     * @return the machineReadableUrl
     */
    public final URL getMachineReadableUrl() {
        return machineReadableUrl;
    }

    /**
     * @param machineReadableUrl
     *         the machineReadableUrl to set
     *
     * @return this instance for chaining
     */
    public final Publication setMachineReadableUrl(final URL machineReadableUrl) {
        this.machineReadableUrl = machineReadableUrl;
        return this;
    }

    /**
     * @return the humanReadableUrl
     */
    public final URL getHumanReadableUrl() {
        return humanReadableUrl;
    }

    /**
     * @param humanReadableUrl
     *         the humanReadableUrl to set
     *
     * @return this instance for chaining
     */
    public final Publication setHumanReadableUrl(final URL humanReadableUrl) {
        this.humanReadableUrl = humanReadableUrl;
        return this;
    }

    /**
     * @return the publicationDate
     */
    public final LocalDate getPublicationDate() {
        return publicationDate;
    }

    /**
     * @param publicationDate
     *         the publicationDate to set
     *
     * @return this instance for chaining
     */
    public final Publication setPublicationDate(final LocalDate publicationDate) {
        this.publicationDate = publicationDate;
        return this;
    }

    /**
     * @return the dispatchDate
     */
    public final LocalDate getDispatchDate() {
        return dispatchDate;
    }

    /**
     * @param dispatchDate
     *         the dispatchDate to set
     *
     * @return this instance for chaining
     */
    public final Publication setDispatchDate(final LocalDate dispatchDate) {
        this.dispatchDate = dispatchDate;
        return this;
    }

    /**
     * @return the language
     */
    public final String getLanguage() {
        return language;
    }

    /**
     * @param language
     *         the language to set
     *
     * @return this instance for chaining
     */
    public final Publication setLanguage(final String language) {
        this.language = language;
        return this;
    }

    /**
     * @return the version
     */
    public final Integer getVersion() {
        return version;
    }

    /**
     * @param version
     *         the version to set
     *
     * @return this instance for chaining
     */
    public final Publication setVersion(final Integer version) {
        this.version = version;
        return this;
    }

    /**
     * @return the isValid
     */
    public final Boolean getIsValid() {
        return isValid;
    }

    /**
     * @param isValid
     *         the isValid to set
     *
     * @return this instance for chaining
     */
    public final Publication setIsValid(final Boolean isValid) {
        this.isValid = isValid;
        return this;
    }

    /**
     * @return the isIncluded
     */
    public final Boolean getIsIncluded() {
        return isIncluded;
    }

    /**
     * @param isIncluded
     *         the isIncluded to set
     *
     * @return this instance for chaining
     */
    public final Publication setIsIncluded(final Boolean isIncluded) {
        this.isIncluded = isIncluded;
        return this;
    }

    /**
     * @return information on whether the announcement was obligatory to be published
     */
    public final Boolean getIsMandatory() {
        return isMandatory;
    }

    /**
     * @param isMandatory
     *         information on whether the announcement was obligatory to be published
     *
     * @return this instance for chaining
     */
    public final Publication setIsMandatory(final Boolean isMandatory) {
        this.isMandatory = isMandatory;
        return this;
    }

    /**
     * @return the formType
     */
    public final PublicationFormType getFormType() {
        return formType;
    }

    /**
     * @param formType
     *         the formType to set
     *
     * @return this instance for chaining
     */
    public final Publication setFormType(final PublicationFormType formType) {
        this.formType = formType;
        return this;
    }

    /**
     * @return the sourceFormType
     */
    public final String getSourceFormType() {
        return sourceFormType;
    }

    /**
     * @param sourceFormType
     *         the sourceFormType to set
     *
     * @return this instance for chaining
     */
    public final Publication setSourceFormType(final String sourceFormType) {
        this.sourceFormType = sourceFormType;
        return this;
    }

    /**
     * @return the lastUpdate
     */
    public final LocalDate getLastUpdate() {
        return lastUpdate;
    }

    /**
     * @param lastUpdate
     *         the lastUpdate to set
     *
     * @return this instance for chaining
     */
    public final Publication setLastUpdate(final LocalDate lastUpdate) {
        this.lastUpdate = lastUpdate;
        return this;
    }

    /**
     * @return the isParentContract
     */
    public final Boolean getIsParentTender() {
        return isParentTender;
    }

    /**
     * @param isParentTender
     *         the isParentTender to set
     *
     * @return this instance for chaining
     */
    public final Publication setIsParentTender(final Boolean isParentTender) {
        this.isParentTender = isParentTender;
        return this;
    }

    @Override
    @JsonIgnore
    public final Publication getValid() {
        return ValidationUtils.getValid(this, source);
    }
}
