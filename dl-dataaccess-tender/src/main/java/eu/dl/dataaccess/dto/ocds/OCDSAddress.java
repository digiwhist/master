package eu.dl.dataaccess.dto.ocds;


import com.fasterxml.jackson.annotation.JsonProperty;
import eu.dl.dataaccess.annotation.Transformable;

/**
 * OCDS address. This object doesn't cover full OCDS schema.
 * 
 * @see <a href="http://standard.open-contracting.org/1.1/en/schema/release/">OCDS Release Schema</a>
 */
@Transformable
public class OCDSAddress {

    private String street;

    private String locality;

    private String region;

    private String postcode;

    private String country;

    /**
     * @return street
     */
    @JsonProperty("streetAddress")
    public final String getStreet() {
        return street;
    }

    /**
     * @param street
     *      street to be set
     * @return this instance for chaining
     */
    public final OCDSAddress setStreet(final String street) {
        this.street = street;
        return this;
    }

    /**
     * @return locality
     */
    public final String getLocality() {
        return locality;
    }

    /**
     * @param locality
     *      localoty to be set
     * @return this instance for chaining
     */
    public final OCDSAddress setLocality(final String locality) {
        this.locality = locality;
        return this;
    }

    /**
     * @return region
     */
    public final String getRegion() {
        return region;
    }

    /**
     * @param region
     *      region to be set
     * @return this instance for chaining
     */
    public final OCDSAddress setRegion(final String region) {
        this.region = region;
        return this;
    }

    /**
     * @return postal code
     */
    @JsonProperty("postalCode")
    public final String getPostcode() {
        return postcode;
    }

    /**
     * @param postcode
     *      postal code to be set
     * @return this instance for chaining
     */
    public final OCDSAddress setPostcode(final String postcode) {
        this.postcode = postcode;
        return this;
    }

    /**
     * @return country name
     */
    @JsonProperty("countryName")
    public final String getCountry() {
        return country;
    }

    /**
     * @param country
     *      country name to be set
     * @return this instance for chaining
     */
    public final OCDSAddress setCountry(final String country) {
        this.country = country;
        return this;
    }
}
