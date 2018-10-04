package eu.dl.dataaccess.dao;

import java.util.List;
import java.util.Map;

import eu.dl.dataaccess.dto.codetables.BodyType;
import eu.dl.dataaccess.dto.matched.MatchedBody;

/**
 * DAO for matched body.
 * 
 * @param <T>
 *            implementation class type that should be used for matched tender
 */
public interface MatchedBodyDAO<T extends MatchedBody>
    extends MatchedDAO<T>, ExactMatchBodyDAO<T>, ApproximateMatchBodyDAO<T> {
    
    /**
     * Saves given tender to persistent storage.
     *
     * @param matchedBody
     *            matched body data to be saved
     *
     * @return Id of saved matched body
     */
    String save(T matchedBody);

    /**
     * Returns the object by given id.
     *
     * @param id
     *            id to be searched
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
     * Returns object with the same etalon id.
     *
     * @param id
     *            etalon id
     *
     * @return matched body with the same etalon id
     */
    T getByEtalonId(String id);

    /**
     * Returns objects which have been stored by the particular version of the matcher. The objects are used to resend
     * so they have to contain group ID.
     *
     * @param name
     *            matcher name
     * @param version
     *            matcher version
     * @return set of objects with only one attribute id and one attribute group ID having set.
     */
    List<T> getForResend(String name, String version);
    
    /**
     * Returns ids of groups with etalon entry.
	 *
     * @return found group ids
     */
    List<String> getEtalonGroupIds();
    
    /**
     * Returns hashs and group pairs.
	 *
     * @return found hashes and group ids
     */
    Map<String, String> getHashAndGroupIds();

    /**
     * Returns list of bodies with the given name and role.
     *
     * @param name
     *          body name
     * @param role
     *          body role
     * @return list of bodies
     */
    List<T> getByNameAndRole(String name, BodyType role);
}
