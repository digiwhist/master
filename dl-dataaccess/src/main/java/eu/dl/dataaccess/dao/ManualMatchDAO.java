package eu.dl.dataaccess.dao;

import java.util.List;

import eu.dl.dataaccess.dto.matched.ManualMatch;

/**
 * DAO for manual matches.
 * 
 * @param <T>
 *            manual match result
 */
public interface ManualMatchDAO<T> {
    /**
     * Returns matches for the hash.
     * 
     * @param hash
     *            hash to be searched
     * @param flag
     *            flag/type of the item
     * 
     * @return list of objects with the hash
     */
    List<T> getByHash(String hash, String flag);

    /**
     * Returns all stored matches for a flag.
     * 
     * @param flag flag/type of the item
     * 
     * @return list of objects
     */
	List<ManualMatch> getAllEntries(String flag);
}
