package eu.dl.dataaccess.dto.parsed;

import java.util.ArrayList;
import java.util.List;

// TODO: Auto-generated Javadoc
/**
 * Address.
 */
public class ParsedAddress {
    private String street;
    private String city;
    private String postcode;
    private List<String> nuts;
    private String country;
    private String rawAddress;
    private String state;
    private String url;

    /**
     * Gets the street.
     *
     * @return the street
     */
    public final String getStreet() {
        return street;
    }

    /**
     * Sets the street.
     *
     * @param street
     *            the street
     * @return the parsed address
     */
    public final ParsedAddress setStreet(final String street) {
        this.street = street;
        return this;
    }

    /**
     * Gets the city.
     *
     * @return the city
     */
    public final String getCity() {
        return city;
    }

    /**
     * Sets the city.
     *
     * @param city
     *            the city
     * @return the parsed address
     */
    public final ParsedAddress setCity(final String city) {
        this.city = city;
        return this;
    }

    /**
     * Gets the postcode.
     *
     * @return the postcode
     */
    public final String getPostcode() {
        return postcode;
    }

    /**
     * Sets the postcode.
     *
     * @param postcode
     *            the postcode
     * @return the parsed address
     */
    public final ParsedAddress setPostcode(final String postcode) {
        this.postcode = postcode;
        return this;
    }

    /**
     * Gets the nuts.
     *
     * @return the nuts
     */
    public final List<String> getNuts() {
        return nuts;
    }

    /**
     * Sets the nuts.
     *
     * @param nuts
     *            the nuts
     * @return the parsed address
     */
    public final ParsedAddress setNuts(final List<String> nuts) {
        this.nuts = nuts;
        return this;
    }

    /**
     * Gets the country.
     *
     * @return the country
     */
    public final String getCountry() {
        return country;
    }

    /**
     * Sets the country.
     *
     * @param country
     *            the country
     * @return the parsed address
     */
    public final ParsedAddress setCountry(final String country) {
        this.country = country;
        return this;
    }

    /**
     * Gets the raw address.
     *
     * @return the raw address
     */
    public final String getRawAddress() {
        return rawAddress;
    }

    /**
     * Sets the raw address.
     *
     * @param rawAddress
     *            the raw address
     * @return the parsed address
     */
    public final ParsedAddress setRawAddress(final String rawAddress) {
        this.rawAddress = rawAddress;
        return this;
    }

    /**
     * Adds the nuts.
     *
     * @param nutsCode
     *            the nuts code
     * @return the parsed address
     */
    public final ParsedAddress addNuts(final String nutsCode) {
        if (nutsCode != null) {
            if (getNuts() == null) {
                setNuts(new ArrayList<>());
            }
            this.nuts.add(nutsCode);
        }

        return this;
    }

    /**
     * Gets the state.
     *
     * @return the state
     */
    public final String getState() {
        return state;
    }

    /**
     * Sets the state.
     *
     * @param state
     *            the state
     * @return the parsed address
     */
    public final ParsedAddress setState(final String state) {
        this.state = state;
        return this;
    }

    /**
     * Gets url.
     *
     * @return the url
     */
    public final String getUrl() {
        return url;
    }

    /**
     * Sets url.
     *
     * @param url the url
     *
     * @return the url
     */
    public final ParsedAddress setUrl(final String url) {
        this.url = url;
        return this;
    }
}
