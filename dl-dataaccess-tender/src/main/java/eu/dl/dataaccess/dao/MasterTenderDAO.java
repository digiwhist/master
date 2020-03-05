package eu.dl.dataaccess.dao;

import eu.dl.dataaccess.dto.master.MasterTender;

import java.time.LocalDate;
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
     * Returns objects which has been modified after timestamp. The result is paged with {@code pageSize} records per page.
     *
     * @param timestamp
     *            objects modified after this timestamp will be returned
     * @param page
     *            order of the page in the result
     * @param pageSize
     *      page size
     * @return set of objects modified after timestamp
     */
    List<T> getModifiedAfter(LocalDateTime timestamp, Integer page, Integer pageSize);

    /**
     * Same as {@link MasterTenderDAO#getModifiedAfter(LocalDateTime, Integer, Integer)} but uses default page size.
     *
     * @param timestamp
     *            objects modified after this timestamp will be returned
     * @param page
     *            order of the page in the result
     * @return set of objects modified after timestamp
     */
    List<T> getModifiedAfter(LocalDateTime timestamp, Integer page);

    /**
     * Same as {@link MasterTenderDAO#getByCountry(String, Integer, Integer)} nut uses default page size.
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
     * Returns paged list of master items for a specific country.
     *
     * @param countryCode
     *            ISO country code
     * @param page
     *            page number
     * @param pageSize
     *      page size
     * @return paged list of master items from given country
     */
    List<T> getByCountry(String countryCode, Integer page, Integer pageSize);

    /**
     * Same as {@link MasterTenderDAO#getByCountry(String, Integer, String, Integer)} nut uses default page size.
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
     *            ISO country code
     * @param page
     *            page number
     * @param source
     *          source
     * @param pageSize
     *      page size
     * @return paged list of master items from given country and source
     */
    List<T> getByCountry(String countryCode, Integer page, String source, Integer pageSize);

    /**
     * Returns list of tender ids modified by source and version. The IDs are sorted.
     *
     * @param name worker name
     * @param version worker version
     *
     * @return list of tender ids
     */
    List<String> getIdsBySourceAndVersion(String name, String version);

    /**
     * Returns last date of publication before {@code maxDate} for the given worker and version.
     *
     * @param createdBy
     *      worker name
     * @param createdByVersion
     *      worker version
     * @param maxDate
     *      max date
     * @return last publication date or null
     */
    LocalDate getLastPublicationDate(String createdBy, String createdByVersion, LocalDate maxDate);

    /**
     * Returns last date of publication for the given worker and version.
     *
     * @param createdBy
     *      worker name
     * @param createdByVersion
     *      worker version
     *
     * @return last publication date or null
     */
    LocalDate getLastPublicationDate(String createdBy, String createdByVersion);
}
