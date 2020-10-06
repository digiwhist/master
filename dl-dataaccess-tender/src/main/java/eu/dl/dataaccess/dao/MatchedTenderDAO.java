package eu.dl.dataaccess.dao;

import eu.dl.dataaccess.dto.matched.MatchedTender;

import java.net.URL;
import java.time.LocalDate;
import java.util.List;
import java.util.Map;

/**
 * DAO for matched body.
 *
 * @param <T>
 *         implementation class type that should be used for clean tender
 */
public interface MatchedTenderDAO<T extends MatchedTender> extends MatchedDAO<T> {
    /**
     * Saves given tender to persistent storage.
     *
     * @param matchedTender
     *         matched tender data to be saved
     *
     * @return Id of saved matched body
     */
    String save(T matchedTender);

    /**
     * Returns the object by given id.
     *
     * @param id
     *         id to be searched
     *
     * @return matched body with given id
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
     * Returns list of tenders that have non-empty intersection with the given list of tender publications source ids.
     *
     * @param publicationSourceIds
     *         distinct list of clean tender publications source ids
     *
     * @return list of tenders
     */
    List<T> getByPublicationSourceIds(List<String> publicationSourceIds);

    /**
     * Returns tenders which have non-empty intersection with the given list of publication human readable URLs.
     *
     * @param publicationHumanReadableUrls
     *         publication human readable URLs
     *
     * @return list of tenders
     */
    List<T> getByPublicationHumanReadableUrls(List<URL> publicationHumanReadableUrls);

    /**
     * Returns tenders which have non-empty intersection with the given list of publication machine readable URLs.
     *
     * @param publicationMachineReadableUrls
     *         publication machine readable URLs
     *
     * @return list of tenders
     */
    List<T> getByPublicationMachineReadableUrls(List<URL> publicationMachineReadableUrls);

    /**
     * Returns tenders which have the same documents URL.
     *
     * @param documentsUrl
     *         documents URL
     *
     * @return list of tenders
     */
    List<T> getByDocumentsUrl(URL documentsUrl);

    /**
     * Returns objects which have been stored by the particular version of the matcher. The objects are used to resend
     * so they have to contain group ID.
     *
     * @param name
     *         matcher name
     * @param version
     *         matcher version
     *
     * @return set of objects with only one attribute id and one attribute group ID having set.
     */
    List<T> getForResend(String name, String version);

    /**
     * Returns list of tenders that have non-empty intersection with the given list of tender publications source ids
     * and publication dates.
     *
     * @param sourceIdsAndDates
     *         map of clean tender publications source ids and publication dates
     *
     * @return list of tenders
     */
    List<T> getByPublicationSourceIdsAndPublicationDates(Map<String, LocalDate> sourceIdsAndDates);
}
