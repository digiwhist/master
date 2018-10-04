package eu.dl.dataaccess.dao;

import eu.dl.dataaccess.dto.clean.CleanTender;

import java.time.LocalDate;
import java.util.List;

/**
 * Clean tender DAO interface. Specifies methods for manipulating data about tenders that has been parsed and
 * converted to correct data types.
 *
 * @param <T>
 *         implementation class type that should be used for clean tender
 */
public interface CleanTenderDAO<T extends CleanTender> extends CleanDAO<T> {

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
     * Returns paged list of clean items for a specific country.
     *
     * @param countryCode
     *            ISO country code
     * @param page
     *            page number
     *
     * @return paged list of clean items from given country
     */
    List<T> getByCountry(String countryCode, Integer page);

    /**
     * Gets list of publication source IDs which are included.
     *
     * @param date
     *            publication date
     *
     * @return list publication source IDs which are included.
     */
    List<String> getIncludedPublicationSourceIds(LocalDate date);
}
