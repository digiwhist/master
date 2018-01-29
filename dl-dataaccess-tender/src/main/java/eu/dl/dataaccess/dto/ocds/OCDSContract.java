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
}
