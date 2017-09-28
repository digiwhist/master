package eu.dl.dataaccess.dao.mongo;

import java.time.LocalDateTime;
import java.util.List;

/**
 * @param <T>
 *            one of DTOs
 * @author Kuba Krafka
 */
public interface MongoDAO<T> {
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
     * Returns random object modified by the source.
     *
     * @param source
     *            source name
     * @return found object
     */
    T getRandom(String source);

    /**
     * Set source name to be used for metadata info.
     *
     * @param sourceName
     *            name of the source(worker for example)
     */
    void setWorkerName(String sourceName);

    /**
     * Set source version to be used in metadata.
     *
     * @param sourceVersion
     *            version of the source(worker for example)
     */
    void setWorkerVersion(String sourceVersion);

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
     * Returns objects which has been deleted after timestamp. The result is
     * paged with 1000 records per page.
     *
     * @param timestamp
     *            objects deleted after this timestamp will be returned
     * @param page
     *            order of the page in the result
     * 
     * @return set of objects deleted after timestamp
     */
    List<T> getDeletedAfter(LocalDateTime timestamp, Integer page);

    /**
     * Deletes tender with given id.
     *
     * @param id
     *            id of tender to be deleted
     */
    void delete(String id);
}
