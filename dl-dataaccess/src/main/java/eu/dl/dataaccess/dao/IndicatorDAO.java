package eu.dl.dataaccess.dao;

import java.util.Collection;
import java.util.List;

/**
 * Indicator DAO used to handle Indicators/red flags DTOs.
 * 
 * @param <T>
 *            implementation class type that should be used for indicator.
 */
public interface IndicatorDAO<T> {

    /**
     * Deletes existing indicator for item with given id.
     * 
     * @param itemId
     *            for which item should be indicator deleted
     * 
     * @param indicatorType
     *            which indicator type should be deleted
     * 
     */
    void delete(String itemId, String indicatorType);

    /**
     * Saves indicator.
     *
     * @param indicator
     *            indicator to be saved
     *
     * @return Id of saved indicator
     */
    String save(T indicator);

    /**
     * Returns list of indicators related to entity.
     * @param id entity id
     * @return indicators
     */
    List<T> getByEntityId(String id);
    
    /**
     * Returns list of indicators related to entity.
     * @param ids entity ids
     * @return indicators
     */
    List<T> getByEntityIds(Collection<String> ids);
}
