package eu.dl.dataaccess.dao;

import eu.dl.dataaccess.dto.master.MasterBody;

import java.time.LocalDateTime;
import java.util.List;

/**
 * DAO for mastered body.
 *
 * @param <T>
 *         implementation class type that should be used for mastered tender
 */
public interface MasterBodyDAO<T extends MasterBody> extends MasterDAO<T> {
    /**
     * Saves the contact.
     *
     * @param item
     *         item to be saved
     *
     * @return Id of saved item
     */
    String save(T item);

    /**
     * Returns item with id.
     *
     * @param id
     *         item id
     *
     * @return Clean item with given id
     */
    T getById(String id);

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
     *         objects modified after this timestamp will be returned
     * @param page
     *         order of the page in the result
     *
     * @return set of objects modified after timestamp
     */
    List<T> getModifiedAfter(LocalDateTime timestamp, Integer page);

    /**
     * Returns objects which has been modified after timestamp by certain
     * source. The result is paged with 1000 records per page.
     *
     * @param timestamp
     *         objects modified after this timestamp will be returned
     * @param modifiedBy
     *         "author" of the change
     * @param page
     *         order of the page in the result
     *
     * @return set of objects modified after timestamp
     */
    List<T> getModifiedAfter(LocalDateTime timestamp, String modifiedBy, Integer page);

    /**
     * Returns new instance of T.
     *
     * @return empty instance
     */
    T getEmptyInstance();

    /**
     * Find out if BVD id is in table political exposed persons.
     *
     * @param bvdIdNumber bvd id number to search for
     *
     * @return boolean
     */
    boolean existsInPoliticalExposedPersons(String bvdIdNumber);
}
