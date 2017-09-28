package eu.dl.dataaccess.dao;

import java.util.List;

import eu.dl.dataaccess.dto.raw.Raw;

/**
 * Raw data DAO interface. Specifies methods for storing and loading raw data
 * (source data like HTML or XML code).
 * 
 * @param <T>
 *            implementation class type that should be used for raw item
 */
public interface RawDAO<T extends Raw> {
    // signatures of methods for CRUD operations on parsed items

    /**
     * Saves given item to persistent storage.
     *
     * @param parsedItem
     *            parsed item data to be saved
     *
     * @return Id of saved item
     */
    String save(T parsedItem);

    /**
     * Returns the object by given id.
     *
     * @param id
     *            id to be searched
     *
     * @return item with given id
     */
    T getById(String id);

    /**
     * Returns the object by source, version and source URL.
     *
     * @param name worker name
     * @param version worker version
     * @param sourceUrl source URL of record
     *
     * @return item with given id
     */
    T getBySourceUrl(String name, String version, String sourceUrl);

    /**
     * Returns ids of object which has been stored by the particular version of the worker.
     *
     * @param name
     *            worker name
     * @param version
     *            worker version
     * @param fromDate
     *            from date
     * @param toDate
     *            to date
     *
     * @return set of ids
     */
    List<T> getMine(String name, String version, String fromDate, String toDate);

    /**
     * Returns new instance of T.
     *
     * @return empty instance
     */
    T getEmptyInstance();

    /**
     * Returns list of ids modified by source and version. The IDs are sorted.
     *
     * @param name worker name
     * @param version worker version
     *
     * @return list of ids
     */
    List<String> getIdsBySourceAndVersion(String name, String version);
}
