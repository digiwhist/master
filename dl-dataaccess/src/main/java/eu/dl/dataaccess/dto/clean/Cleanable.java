package eu.dl.dataaccess.dto.clean;

import eu.dl.dataaccess.dto.Storable;

/**
 * Shared interface for all cleanable items.
 *
 */
public interface Cleanable extends Storable, Validable {
    /**
     * @return the id
     */
    String getId();

    /**
     * @param id
     *            the id to set
     * 
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
     *
     * @return reference to id of the associated parsed object
     */
    String getParsedObjectId();

    /**
     *
     * @param parsedObjectId
     *            id of the associated parsed object in parsed collection
     */
    void setParsedObjectId(String parsedObjectId);
    
    /**
     * @return the persistentId
     */
    String getPersistentId();

    /**
     * @param persistentId the persistentId to set
     */
    void setPersistentId(String persistentId);
}

