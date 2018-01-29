package eu.dl.dataaccess.dto.master;

import java.util.List;

import eu.dl.dataaccess.dto.Storable;
import eu.dl.dataaccess.dto.indicator.Indicator;

/**
 * Shared interface for all masterable items.
 */
public interface Masterable extends Storable {
    /**
     * @return the groupId
     */
    String getGroupId();

    /**
     * @param groupId
     *            the groupId to set
     * 
     * @return this instance for chaining
     */
    Masterable setGroupId(String groupId);
    
    /**
     * List of indicators relevant for this entity.
     * 
     * @return the indicators
     */
    List<Indicator> getIndicators();

    /**
     * Set indicators relevant for this entity.
     * 
     * @param indicators the indicators to set
     * 
     * @return the master tender
     */
    Masterable setIndicators(List<Indicator> indicators);
}
