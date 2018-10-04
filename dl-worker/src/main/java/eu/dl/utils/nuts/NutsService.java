package eu.dl.utils.nuts;

/**
 * Provides postcode to NUTS conversion functionality.
 * 
 * @author Tomas Mrazek
 */
public interface NutsService {
    /**
     * Returns NUTS code for the given postcode and country.
     *
     * @param country
     *      country
     * @param postcode
     *      postcode
     * @return NUTS or NULL
     */
    String convert(String country, String postcode);
}
