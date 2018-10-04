package eu.dl.dataaccess.dto.ocds;


import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import eu.dl.dataaccess.annotation.Transformable;
import java.time.LocalDateTime;

/**
 * OCDS contract. This object doesn't cover full OCDS schema.
 * 
 * @see <a href="http://standard.open-contracting.org/1.1/en/schema/release/">OCDS Release Schema</a>
 */
@Transformable
public class OCDSContract extends BaseOCDSDocumentsReferrer<OCDSContract> {

    private String id;

    private String awardId;

    private LocalDateTime signed;
    
    private OCDSImplementation implementation;

    private String title;

    private String description;

    private String status;

    private OCDSPeriod period;

    private OCDSValue value;

    /**
     * @return id
     */
    public final String getId() {
        return id;
    }

    /**
     * @param id
     *      id to be set
     * @return this instance for chaining
     */
    public final OCDSContract setId(final String id) {
        this.id = id;
        return this;
    }

    /**
     * @return award id
     */
    @JsonProperty("awardID")
    public final String getAwardId() {
        return awardId;
    }

    /**
     * @param awardId
     *      award id to be set
     * @return this instance for chaining
     */
    public final OCDSContract setAwardId(final String awardId) {
        this.awardId = awardId;
        return this;
    }

    /**
     * @return date of signing
     */
    @JsonProperty("dateSigned")
    @JsonSerialize(using = OCDSLocalDateTimeSerializer.class)
    public final LocalDateTime getSigned() {
        return signed;
    }

    /**
     * @param signed
     *      signing date to be set
     * @return this instance for chaining
     */
    public final OCDSContract setSigned(final LocalDateTime signed) {
        this.signed = signed;
        return this;
    }

    /**
     * @return implementation
     */
    public final OCDSImplementation getImplementation() {
        return implementation;
    }

    /**
     * @param implementation
     *      implementation to be set
     * @return this instance for chaining
     */
    public final OCDSContract setImplementation(final OCDSImplementation implementation) {
        this.implementation = implementation;
        return this;
    }

    /**
     * @return description
     */
    public final String getDescription() {
        return description;
    }

    /**
     * @param description
     *      description to be set
     * @return this instance for chaining
     */
    public final OCDSContract setDescription(final String description) {
        this.description = description;
        return this;
    }

    /**
     * @return status
     */
    public final String getStatus() {
        return status;
    }

    /**
     * @param status
     *      status to be set
     * @return this instance for chaining
     */
    public final OCDSContract setStatus(final String status) {
        this.status = status;
        return this;
    }

    /**
     * @return title
     */
    public final String getTitle() {
        return title;
    }

    /**
     * @param title
     *      title to be set
     * @return this instance for chaining
     */
    public final OCDSContract setTitle(final String title) {
        this.title = title;
        return this;
    }

    /**
     * @return period
     */
    public final OCDSPeriod getPeriod() {
        return period;
    }

    /**
     * @param period
     *      period to be set
     * @return this instance for chaining
     */
    public final OCDSContract setPeriod(final OCDSPeriod period) {
        this.period = period;
        return this;
    }

    /**
     * @return value
     */
    public final OCDSValue getValue() {
        return value;
    }

    /**
     * @param value
     *      value to be set
     * @return this instance for chaining
     */
    public final OCDSContract setValue(final OCDSValue value) {
        this.value = value;
        return this;
    }
}
