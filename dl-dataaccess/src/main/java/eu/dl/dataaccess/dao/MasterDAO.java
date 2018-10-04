package eu.dl.dataaccess.dao;

import java.time.LocalDateTime;
import java.util.Collection;
import java.util.List;

/**
 * Shared methods for master DAOs.
 *
 * @param <T>
 *            mastered item
 */
public interface MasterDAO<T> {
    /**
     * Returns objects with the same groupId.
     * 
     * @param groupId
     *            group id to be searched
     * 
     * @return list of objects with the specific group id
     */
    List<T> getByGroupId(String groupId);

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
     * Saves given item to persistent storage.
     *
     * @param masteredItem
     *            mastered item to be saved
     *
     * @return Id of saved item
     */
    String save(T masteredItem);

    /**
     * Returns objects which has been modified after timestamp. The result is
     * paged with 1000 records per page.
     *
     * @param timestamp
     *            objects modified after this timestamp will be returned
     * @param page
     *            order of the page in the result
     *
     * @return set of objects modified after timestamp
     */
    List<T> getModifiedAfter(LocalDateTime timestamp, Integer page);

    /**
     * Returns objects which has been modified after timestamp by certain
     * source. The result is paged with 1000 records per page.
     *
     * @param timestamp
     *            objects modified after this timestamp will be returned
     * @param modifiedBy
     *            "author" of the change
     * @param page
     *            order of the page in the result
     *
     * @return set of objects modified after timestamp
     */
    List<T> getModifiedAfter(LocalDateTime timestamp, String modifiedBy, Integer page);


    /**
     * Returns objects which has been modified after timestamp by certain
     * source. The result is paged with 1000 records per page.
     *
     * @param timestamp
     *            objects modified after this timestamp will be returned
     * @param createdBy
     *            "author" of the change
     * @param country
     *            country the tender is coming from
     * @param page
     *            order of the page in the result
     *
     * @return set of objects modified after timestamp
     */
    List<T> getModifiedAfter(LocalDateTime timestamp, String createdBy, String country, Integer page);

    /**
     * Returns new instance of T.
     *
     * @return empty instance
     */
    T getEmptyInstance();
}
