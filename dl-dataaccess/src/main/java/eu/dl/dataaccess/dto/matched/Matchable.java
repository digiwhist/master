package eu.dl.dataaccess.dto.matched;
import eu.dl.dataaccess.dto.Storable;

/**
 * Shared interface for all matchable items.
 */
public interface Matchable extends Storable {
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
    Matchable setGroupId(String groupId);
}