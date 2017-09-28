package eu.dl.dataaccess.dto.parsed;

import eu.dl.dataaccess.dto.Storable;

/**
 * Shared interface for all parsable items.
 */
public interface Parsable extends Storable {
    /**
     * @return the id
     */
    String getId();

    /**
     * @param id
     *            the id to set
     */
    void setId(String id);

    /**
     *
     * @return reference to id of the associated raw object
     */
    String getRawObjectId();

    /**
     *
     * @param rawObjectId
     *            id of the associated parsed object in raw collection
     */
    void setRawObjectId(String rawObjectId);
    
    /**
     * @return the persistentId
     */
    String getPersistentId();

    /**
     * @param persistentId the persistentId to set
     */
    void setPersistentId(String persistentId);
}

