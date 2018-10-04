package eu.dl.dataaccess.dao;

import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.matched.PoolBody;

import java.util.List;

/**
 * Shared methods for approximatelly matchable body DAOs.
 * 
 * @param <T>
 *            implementation class type that should be used for approximatelly matchable body
 */
public interface ApproximateMatchBodyDAO<T extends PoolBody> extends ExactMatchBodyDAO<T> {
    /**
     * Returns objects for body approximate matching.
     *
     * @param standardizedName
     *          standardized name
     * @param standardizedAddress
     *          standardized address
     * @param bodyIds
     *          list of body identifiers
     * @param digest
     *      body digest string
     * @return list of matched bodies, empty list for no results
     */
    List<T> getApproximateMatchBodiesPool(String standardizedName, String standardizedAddress,
        List<BodyIdentifier> bodyIds, String digest);
}
