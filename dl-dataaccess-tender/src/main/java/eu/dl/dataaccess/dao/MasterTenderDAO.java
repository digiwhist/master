package eu.dl.dataaccess.dao;

import eu.dl.dataaccess.dto.codetables.PublicationFormType;
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
     * Returns list of tender ids modified by source and version. The IDs are sorted.
     *
     * @param name worker name
     * @param version worker version
     *
     * @return list of tender ids
     */
    List<String> getIdsBySourceAndVersion(String name, String version);

    /**
     * Returns all tenders for the given buyer group id which have the first included publication of the given
     * {@code formType} published between {@code from} and {@code to} dates.
     *
     * @param buyerGroupId
     *      buyer group id
     * @param from
     *      from date
     * @param to
     *      to date
     * @param formType
     *      form type of included publication
     * @return list of buyer's tenders
     */
    List<T> getBuyerTendersInPeriod(String buyerGroupId, LocalDate from, LocalDate to, PublicationFormType formType);

    /**
     * Returns median of the given CPV for tenders of the given type published in the given period.
     *
     * @param createdBy
     *      worker name
     * @param from
     *      start of the period
     * @param to
     *      end of the period
     * @param formType
     *      form type of included publication
     * @param cpv
     *      CPV for that the median to be calculated
     * @return array where index 0 holds median value for the given CPV and index 1 holds number of tenders used for median calculation
     */
    int[] getCPVMedianInPeriod(List<String> createdBy, LocalDate from, LocalDate to, PublicationFormType formType, String cpv);
}
