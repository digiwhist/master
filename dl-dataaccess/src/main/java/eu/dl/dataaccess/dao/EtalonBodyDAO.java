package eu.dl.dataaccess.dao;

import java.util.List;

import eu.dl.dataaccess.dto.matched.PoolBody;

/**
 * DAO for etalon body.
 * 
 * @param <T>
 *            implementation class type that should be used for etalon body
 */
public interface EtalonBodyDAO<T extends PoolBody> extends ExactMatchBodyDAO<T>, ApproximateMatchBodyDAO<T> {
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
     * Returns paged etalon bodies ordered by the primary key.
     * 
     * @param pageNumber
     *            number of the page
     * @param pageSize
     *            page size
     * @return set of objects or empty list.
     */
    List<T> findAll(int pageNumber, int pageSize);
    
    /**
     * Returns paged etalon bodies ordered by the primary key. Skips offset
     * amout of results.
     * 
     * @param pageNumber
     *            number of the page
     * @param pageSize
     *            page size
     * @param offset
     *            initial offset
     * @return set of objects or empty list.
     */
    List<T> findAll(int pageNumber, int pageSize, int offset);
    
    /**
     * Returns amount of etalon bodies ordered by the primary key with id higher than param id.
     * 
     * @param id
     *            number of the page
     * @param amount
     *            amount of entries to be returned
     *
     * @return set of objects or empty list.
     */
    List<T> findAllById(int id, int amount);

    /**
     * Updates etalon entry in database.
     *
     * @param body
     *      etalon body
     */
    void updateDigestsAndBodyIdsAndNuts(T body);
}
