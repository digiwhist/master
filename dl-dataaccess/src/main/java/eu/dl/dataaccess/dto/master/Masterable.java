package eu.dl.dataaccess.dto.master;

import eu.dl.dataaccess.dto.Storable;

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
}
