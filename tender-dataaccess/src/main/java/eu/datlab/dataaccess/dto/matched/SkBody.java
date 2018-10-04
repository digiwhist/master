package eu.datlab.dataaccess.dto.matched;

import com.fasterxml.jackson.annotation.JsonProperty;
import eu.dl.dataaccess.dto.StorableDTO;

/**
 * Represents external Slovakia body DB.
 */
public class SkBody extends StorableDTO {
    /**
     * ID of record (starts from 1).
     */
    @JsonProperty("_record_id")
    private Integer recordId;

    /**
     * Probably old official body name.
     */
    @JsonProperty("name_history")
    private String nameHistory;

    /**
     * Official body name.
     */
    private String name;

    /**
     * Organization ID.
     */
    @JsonProperty("ico")
    private String organizationId;

    /**
     * Address.
     */
    private String address;

    /**
     * Legal form.
     */
    @JsonProperty("legal_form")
    private String legalForm;

    /**
     * Region.
     */
    private String region;

    /**
     * Date start.
     */
    @JsonProperty("date_start")
    private String dateStart;

    /**
     * Date end.
     */
    @JsonProperty("date_end")
    private String dateEnd;

    /**
     * Activity 1.
     */
    private String activity1;

    /**
     * Activity 2.
     */
    private String activity2;

    /**
     * Account sector.
     */
    @JsonProperty("account_sector")
    private String accountSector;

    /**
     * Ownership.
     */
    private String ownership;

    /**
     * Size.
     */
    private String size;

    /**
     * Source url.
     */
    @JsonProperty("source_url")
    private String sourceUrl;

    /**
     * @return the record ID
     */
    public final Integer getRecordId() {
        return recordId;
    }

    /**
     * @param recordId
     *         the record ID to set
     * @return this instance for chaining
     */
    public final SkBody setRecordId(final Integer recordId) {
        this.recordId = recordId;
        return this;
    }

    /**
     * @return the name history
     */
    public final String getNameHistory() {
        return nameHistory;
    }

    /**
     * @param nameHistory
     *         the name history to set
     * @return this instance for chaining
     */
    public final SkBody setNameHistory(final String nameHistory) {
        this.nameHistory = nameHistory;
        return this;
    }

    /**
     * @return the name
     */
    public final String getName() {
        return name;
    }

    /**
     * @param name
     *         the name to set
     * @return this instance for chaining
     */
    public final SkBody setName(final String name) {
        this.name = name;
        return this;
    }

    /**
     * @return the organizationId
     */
    public final String getOrganizationId() {
        return organizationId;
    }

    /**
     * @param organizationId
     *         the organizationId to set
     * @return this instance for chaining
     */
    public final SkBody setOrganizationId(final String organizationId) {
        this.organizationId = organizationId;
        return this;
    }

    /**
     * @return the address
     */
    public final String getAddress() {
        return address;
    }

    /**
     * @param address
     *         the address to set
     * @return this instance for chaining
     */
    public final SkBody setAddress(final String address) {
        this.address = address;
        return this;
    }

    /**
     * @return the legal form
     */
    public final String getLegalForm() {
        return legalForm;
    }

    /**
     * @param legalForm
     *         the legal form to set
     * @return this instance for chaining
     */
    public final SkBody setLegalForm(final String legalForm) {
        this.legalForm = legalForm;
        return this;
    }

    /**
     * @return the region
     */
    public final String getRegion() {
        return region;
    }

    /**
     * @param region
     *         the region to set
     * @return this instance for chaining
     */
    public final SkBody setRegion(final String region) {
        this.region = region;
        return this;
    }

    /**
     * @return the date start
     */
    public final String getDateStart() {
        return dateStart;
    }

    /**
     * @param dateStart
     *         the date start to set
     * @return this instance for chaining
     */
    public final SkBody setDateStart(final String dateStart) {
        this.dateStart = dateStart;
        return this;
    }

    /**
     * @return the date end
     */
    public final String getDateEnd() {
        return dateEnd;
    }

    /**
     * @param dateEnd
     *         the date end to set
     * @return this instance for chaining
     */
    public final SkBody setDateEnd(final String dateEnd) {
        this.dateEnd = dateEnd;
        return this;
    }

    /**
     * @return the activity 1
     */
    public final String getActivity1() {
        return activity1;
    }

    /**
     * @param activity1
     *         the activity 1 to set
     * @return this instance for chaining
     */
    public final SkBody setActivity1(final String activity1) {
        this.activity1 = activity1;
        return this;
    }

    /**
     * @return the activity 2
     */
    public final String getActivity2() {
        return activity2;
    }

    /**
     * @param activity2
     *         the activity 2 to set
     * @return this instance for chaining
     */
    public final SkBody setActivity2(final String activity2) {
        this.activity2 = activity2;
        return this;
    }

    /**
     * @return the account sector
     */
    public final String getAccountSector() {
        return accountSector;
    }

    /**
     * @param accountSector
     *         the account sector to set
     * @return this instance for chaining
     */
    public final SkBody setAccountSector(final String accountSector) {
        this.accountSector = accountSector;
        return this;
    }

    /**
     * @return the ownership
     */
    public final String getOwnership() {
        return ownership;
    }

    /**
     * @param ownership
     *         the ownership to set
     * @return this instance for chaining
     */
    public final SkBody setOwnership(final String ownership) {
        this.ownership = ownership;
        return this;
    }

    /**
     * @return the size
     */
    public final String getSize() {
        return size;
    }

    /**
     * @param size
     *         the size to set
     * @return this instance for chaining
     */
    public final SkBody setSize(final String size) {
        this.size = size;
        return this;
    }

    /**
     * @return the source url
     */
    public final String getSourceUrl() {
        return sourceUrl;
    }

    /**
     * @param sourceUrl
     *         the source url to set
     * @return this instance for chaining
     */
    public final SkBody setSourceUrl(final String sourceUrl) {
        this.sourceUrl = sourceUrl;
        return this;
    }
}
