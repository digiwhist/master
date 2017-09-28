package eu.dl.dataaccess.dto.matched;

import java.util.List;

/**
 * Shared interface for all exact matchable bodies. That body should have defined standardized name,
 * standardized address and list of body identifiers.
 */
public interface ApproximatellyMatchable extends ExactellyMatchable {        
    /**
     * Gets the body address postcode.
     *
     * @return the address postcode
     */
    String getPostcode();
    
    /**
     * Gets the body address nuts.
     *
     * @return the address nuts
     */
    List<String> getNuts();
    
    /**
     * Gets body digest.
     *
     * @return digest of the body
     */
    String getDigest();
}