package eu.dl.dataaccess.dao;

import eu.dl.dataaccess.dto.master.MasterTender;

import java.time.LocalDateTime;
import java.util.List;

/**
 * DAO for mastered body.
 *
 * @param <T>
 *         implementation class type that should be used for clean tender
 */
public interface MasterTenderDAO<T extends MasterTender> extends MasterDAO<T> {
    /**
     * Saves given tender to persistent storage.
     *
     * @param masteredTender
     *         mastered tender data to be saved
     *
     * @return Id of saved mastered body
     */
    String save(T masteredTender);

    /**
     * Returns the object by given id.
     *
     * @param id
     *         id to be searched
     *
     * @return mastered body with given id
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
     *            objects modified after this timestamp will be returned (can be null)
     * @param modifiedBy
     *            "author" of the change (can be null)
     * @param page
     *            order of the page in the result (for first page set 0); can be null
     * @param opentender
     *          whether returns only opentender records (tender.metaData.opentender = true)
     * @return set of objects modified after timestamp
     */
    List<T> getModifiedAfter(LocalDateTime timestamp, String modifiedBy, Integer page, boolean opentender);

    /**
     * Returns paged list of master items for a specific country.
     *
     * @param countryCode
     *            ISO country code
     * @param page
     *            page number
     *
     * @return paged list of master items from given country
     */
    List<T> getByCountry(String countryCode, Integer page);

    /**
     * Returns paged list of master items for a specific country and source.
     *
     * @param countryCode
     *            ISO country code
     * @param page
     *            page number
     * @param source
     *          source
     * @return paged list of master items from given country and source
     */
    List<T> getByCountry(String countryCode, Integer page, String source);

    /**
     * Returns paged list of master items for a specific country and source.
     *
     * @param countryCode
     *            ISO country code (can be null)
     * @param page
     *            page number (can be null)
     * @param source
     *          source (can be null)
     * @param opentender
     *          whether returns only opentender records (tender.metaData.opentender = true)
     * @return paged list of master items from given country and source
     */
    List<T> getByCountry(String countryCode, Integer page, String source, boolean opentender);

    /**
     * Returns list of tender ids modified by source and version. The IDs are sorted.
     *
     * @param name worker name
     * @param version worker version
     *
     * @return list of tender ids
     */
    List<String> getIdsBySourceAndVersion(String name, String version);
}
