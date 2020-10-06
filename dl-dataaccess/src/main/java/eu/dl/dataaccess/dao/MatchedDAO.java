package eu.dl.dataaccess.dao;

import java.util.Collection;
import java.util.List;

/**
 * Shared methods for matched DAOs.
 *
 * @param <T>
 *         matched item
 */
public interface MatchedDAO<T> {
    /**
     * Returns objects with the same groupId.
     *
     * @param groupId
     *         group id to be searched
     *
     * @return list of objects with the specific group id
     */
    List<T> getByGroupId(String groupId);

    /**
     * Returns objects with the same ids.
     *
     * @param ids
     *            ids to be searched
     *
     * @return list of objects with the specific ids
     */
    List<T> getByIds(List<String> ids);
    
    /**
     * Returns objects with the same groupId.
     *
     * @param groupIds
     *            group ids to be searched
     *
     * @return list of objects with the specific group ids
     */
    List<T> getByGroupIds(Collection<String> groupIds);

    /**
     * Returns objects with the same hash which have been stored by the particular version of the matcher.
     *
     * @param hash
     *         hash to be searched
     *
     * @return list of objects with the same hash
     */
    List<T> getMineByHash(String hash);

    /**
     * Returns objects with the same hash (if additional workers are set, then also records modified by additional
     * workers are returned).
     *
     * @param hash
     *         hash to be searched
     *
     * @return list of objects with the same hash
     */
    List<T> getByHash(String hash);

    /**
     * Returns new instance of T.
     *
     * @return empty instance
     */
    T getEmptyInstance();
}
