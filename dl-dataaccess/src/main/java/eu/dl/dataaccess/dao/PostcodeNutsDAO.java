package eu.dl.dataaccess.dao;

/**
 * Postcode NUTS DAO interface.
 */
public interface PostcodeNutsDAO {
    /**
     * Returns NUTS for the given postcode and country or null.
     *
     * @param postcode
     *      postcode
     * @param country
     *      country
     * @return NUTS
     */
    String getNutsByPostcode(String postcode, String country);
}
