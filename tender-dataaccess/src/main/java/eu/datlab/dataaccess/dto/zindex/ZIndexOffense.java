package eu.datlab.dataaccess.dto.zindex;

import eu.dl.dataaccess.annotation.Transformable;


/**
 * This class represents the offense record.
 *
 * @author Tomas Mrazek
 */
@Transformable
public final class ZIndexOffense {
    private String organizationId;

    private Integer year;

    private String subject;

    private Boolean isSeriousOffense;

    private Boolean isUOHSConfirmed;
    
    private String url;

    /**
     * @return organization id
     */
    public String getOrganizationId() {
        return organizationId;
    }

    /**
     * @param organizationId
     *      organization id to be set
     * @return this instance for chaining
     */
    public ZIndexOffense setOrganizationId(final String organizationId) {
        this.organizationId = organizationId;
        return this;
    }

    /**
     * @return year
     */
    public Integer getYear() {
        return year;
    }

    /**
     * @param year
     *      year to be set
     * @return this instance for chaining
     */
    public ZIndexOffense setYear(final Integer year) {
        this.year = year;
        return this;
    }

    /**
     * @return subject
     */
    public String getSubject() {
        return subject;
    }

    /**
     * @param subject
     *      subject to be set
     * @return this instance for chaining
     */
    public ZIndexOffense setSubject(final String subject) {
        this.subject = subject;
        return this;
    }

    /**
     * @return whether the offense is serious
     */
    public Boolean getIsSeriousOffense() {
        return isSeriousOffense;
    }

    /**
     * @param seriousOffense
     *      a decision whether the offense is serious to be set
     * @return this instance for chaining
     */
    public ZIndexOffense setIsSeriousOffense(final Boolean seriousOffense) {
        isSeriousOffense = seriousOffense;
        return this;
    }

    /**
     * @return whether the UOHS confirmed offense
     */
    public Boolean getIsUOHSConfirmed() {
        return isUOHSConfirmed;
    }

    /**
     * @param confirmed
     *      an information whether the offense is confirmed by UOHS to be set
     * @return this instance for chaining
     */
    public ZIndexOffense setIsUOHSConfirmed(final Boolean confirmed) {
        isUOHSConfirmed = confirmed;
        return this;
    }
    
    /**
     * @return offense url
     */
    public String getUrl() {
        return url;
    }

    /**
     * @param url
     *      url to be set
     * @return this instance for chaining
     */
    public ZIndexOffense setUrl(final String url) {
        this.url = url;
        return this;
    }
}
