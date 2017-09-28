package eu.dl.dataaccess.dao;

import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.matched.PoolBody;
import java.util.List;

/**
 * Shared methods for body DAOs with digest fields (standardized name, standardized address, etc.).
 * 
 * @param <T>
 *            implementation class type that should be used for digested body
 */
public interface ExactMatchBodyDAO<T extends PoolBody> extends PoolBodyDAO<T> {
    /**
     * Returns objects for body exact matching.
     *
     * @param standardizedName
     *          standardized name
     * @param standardizedAddress
     *          standardized address
     * @param bodyIds
     *          list of body identifiers
     * @return list of matched bodies, empty list for no results
     */
    List<T> getExactMatchBodiesPool(String standardizedName, String standardizedAddress, List<BodyIdentifier> bodyIds);
}
