package eu.dl.dataaccess.dto;

/**
 * Common metadata properties for parsed and clean objects.
 */
public interface ParsedAndCleanStorableDTO {

    /**
     * @return reference to id of the associated raw object
     */
    String getRawObjectId();

    /**
     * @param rawObjectId
     *         id of the associated raw object in raw collection
     */
    void setRawObjectId(String rawObjectId);

}
