package eu.dl.dataaccess.dao;

import java.time.LocalDateTime;
import java.util.List;

/**
 * @param <T>
 *            one of DTOs
 * @author Kuba Krafka
 */
public interface GenericDAO<T> extends BaseDAO<T> {
    /**
     * Saves the object and returns the id.
     *
     * @param t
     *            object to be saved
     * @return saved id
     */
    String save(T t);

    /**
     * Returns object identified by the id.
     *
     * @param id
     *            unique identifier
     * @return found object
     */
    T getById(String id);

    /**
     * Returns objects identified by the ids.
     *
     * @param ids
     *            list of unique identifier
     * @return found objects
     */
    List<T> getByIds(List<String> ids);

    /**
     * Returns objects which has been stored by the particular version of the
     * crawler/downloader.
     *
     * @param name
     *            downloader/crawler name
     * @param version
     *            downloader/crawler version
     * @param fromDate
     *            from date
     * @param toDate
     *            to date
     *
     * @return set of object with only one attribute id having set.
     */
    List<T> getMine(String name, String version, String fromDate, String toDate);

    /**
     * Returns objects which has been modified after timestamp. The result is
     * paged with 1000 records per page.
     *
     * @param timestamp
     *            objects modified after this timestamp will be returned
     * @param page
     *            order of the page in the result (for first page set 0)
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
     *            order of the page in the result (for first page set 0)
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
     * @param countryCode
     *            country code
     * @param page
     *            order of the page in the result (for first page set 0)
     *
     * @return set of objects modified after timestamp
     */
    List<T> getModifiedAfter(LocalDateTime timestamp, String createdBy, String countryCode, Integer page);

    /**
     * Returns count of objects which has been modified after timestamp by certain
     * source.
     *
     * @param timestamp
     *            objects modified after this timestamp will be counted
     * @param createdBy
     *            "author" of the change
     * @param countryCode
     *            country code
     *
     * @return count of objects modified after timestamp
     */
    Integer getModifiedAfterCount(LocalDateTime timestamp, String createdBy, String countryCode);
    
    /**
     * Removes object identified by the id.
     *
     * @param id
     *            unique identifier
     * @return true only and only if the record removing was successful
     */
    Boolean removeById(String id);

    /**
     * Returns list of object ids modified by source and version. The IDs are sorted.
     *
     * @param name worker name
     * @param version worker version
     *
     * @return list of object ids
     */
    List<String> getIdsBySourceAndVersion(String name, String version);
}
