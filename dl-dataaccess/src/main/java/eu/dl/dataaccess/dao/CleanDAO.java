package eu.dl.dataaccess.dao;

import eu.dl.dataaccess.dto.clean.Cleanable;

import java.time.LocalDateTime;
import java.util.List;

/**
 * Clean item DAO interface. Specifies methods for manipulating data about items
 * that has been parsed and converted to correct data types.
 *
 * @param <T>
 *            implementation class type that should be used for clean item
 */
public interface CleanDAO<T extends Cleanable> {
    /**
     * Saves the contact.
     *
     * @param item
     *            item to be saved
     *
     * @return Id of saved item
     */
    String save(T item);

    /**
     * Returns item with id.
     *
     * @param id
     *            item id
     *
     * @return Clean item with given id
     */
    T getById(String id);

    /**
     * Returns objects which has been stored by the particular version of the cleaner.
     *
     * @param name
     *            cleaner name
     * @param version
     *            cleaner version
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
     * Returns new instance of T.
     *
     * @return empty instance
     */
    T getEmptyInstance();
}
