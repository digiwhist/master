package eu.dl.dataaccess.dto.generic;

import com.fasterxml.jackson.annotation.JsonIgnore;
import eu.dl.dataaccess.annotation.Transformable;
import eu.dl.dataaccess.dto.clean.Validable;
import eu.dl.dataaccess.utils.ValidationUtils;

import java.net.URL;
import java.util.ArrayList;
import java.util.List;

/**
 * Address.
 */
@Transformable
public class Address implements Validable {
    private String street;
    private String city;
    private String postcode;
    private List<String> nuts;
    private String country;
    private String rawAddress;
    private String state;
    private URL url;

    /**
     * @return the street
     */

    public final String getStreet() {
        return street;
    }

    /**
     * @param street
     *         the street to set
     * @return this instance for chaining
     */

    public final Address setStreet(final String street) {
        this.street = street;
        return this;
    }

    /**
     * @return the city
     */

    public final String getCity() {
        return city;
    }

    /**
     * @param city
     *         the city to set
     * @return this instance for chaining
     */

    public final Address setCity(final String city) {
        this.city = city;
        return this;
    }

    /**
     * @return the postcode
     */

    public final String getPostcode() {
        return postcode;
    }

    /**
     * @param postcode
     *         the postcode to set
     * @return this instance for chaining
     */

    public final Address setPostcode(final String postcode) {
        this.postcode = postcode;
        return this;
    }

    /**
     * @return the nuts
     */

    public final List<String> getNuts() {
        return nuts;
    }

    /**
     * @param nuts
     *         the nuts to set
     * @return this instance for chaining
     */

    public final Address setNuts(final List<String> nuts) {
        this.nuts = nuts;
        return this;
    }

    /**
     * @return the country
     */

    public final String getCountry() {
        return country;
    }

    /**
     * @param country
     *         the country to set
     * @return this instance for chaining
     */

    public final Address setCountry(final String country) {
        this.country = country;
        return this;
    }

    /**
     * @return the rawAddress
     */

    public final String getRawAddress() {
        return rawAddress;
    }

    /**
     * @param rawAddress
     *         the rawAddress to set
     * @return this instance for chaining
     */

    public final Address setRawAddress(final String rawAddress) {
        this.rawAddress = rawAddress;
        return this;
    }

    /**
     * Adds NUTS code to the list of NUTS codes or create a new list with given NUTS code if none exists.
     *
     * @param nutsCode
     *         new NUTS code to be added
     * @return this instance for chaining
     */

    public final Address addNuts(final String nutsCode) {
        if (getNuts() == null) {
            setNuts(new ArrayList<>());
        }
        this.nuts.add(nutsCode);
        return this;
    }

    /**
     * @return the state
     */
    public final String getState() {
        return state;
    }

    /**
     * @param state
     *            the state to set
     *
     * @return this instance for chaining
     */

    public final Address setState(final String state) {
        this.state = state;
        return this;
    }

    /**
     * Gets url.
     *
     * @return the url
     */
    public final URL getUrl() {
        return url;
    }

    /**
     * Sets url.
     *
     * @param url the url
     *
     * @return the url
     */
    public final Address setUrl(final URL url) {
        this.url = url;
        return this;
    }

    @Override
    @JsonIgnore
    public final Address getValid() {
        setNuts(ValidationUtils.getValid(nuts));

        return ValidationUtils.getValid(this, city, country, nuts, postcode, rawAddress, state, street, url);
    }
}
