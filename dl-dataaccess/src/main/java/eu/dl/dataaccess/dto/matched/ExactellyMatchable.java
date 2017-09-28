package eu.dl.dataaccess.dto.matched;

import java.util.List;

import eu.dl.dataaccess.dto.codetables.BodyIdentifier;

/**
 * Shared interface for all exact matchable bodies. That body should have defined standardized name,
 * standardized address and list of body identifiers.
 */
public interface ExactellyMatchable extends PoolBody {
    
    /**
     * @return standardized name
     */
    String getStandardizedName();

    /**
     * @return standardized address
     */
    String getStandardizedAddress();
    
    /**
     * Gets the body ids.
     *
     * @return the bodyIds
     */
    List<BodyIdentifier> getBodyIds();
}