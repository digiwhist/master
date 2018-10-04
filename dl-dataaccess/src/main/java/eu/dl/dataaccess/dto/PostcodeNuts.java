package eu.dl.dataaccess.dto;

/**
 * Postcode NUTS country specific mapping item.
 * 
 * @author Tomas Mrazek
 */
public class PostcodeNuts {
    private String postcode;

    private String country;

    private String nuts;

    /**
     * @return postcode
     */
    public final String getPostcode() {
        return postcode;
    }

    /**
     * @param postcode
     *      postcode to be set
     * @return this instance for chaining
     */
    public final PostcodeNuts setPostcode(final String postcode) {
        this.postcode = postcode;
        return this;
    }

    /**
     * @return ISO 3166-1 alpha-2 country code
     */
    public final String getCountry() {
        return country;
    }

    /**
     * @param country
     *      ISO 3166-1 alpha-2 country code to be set
     * @return this instance for chaining
     */
    public final PostcodeNuts setCountry(final String country) {
        this.country = country;
        return this;
    }

    /**
     * @return NUTS code
     */
    public final String getNuts() {
        return nuts;
    }

    /**
     * @param nuts
     *      NUTS code to be set
     * @return this instance for chaining
     */
    public final PostcodeNuts setNuts(final String nuts) {
        this.nuts = nuts;
        return this;
    }
}
