package eu.dl.dataaccess.dto.matched;


import eu.dl.dataaccess.dto.BaseParsedAndCleanStorableDTO;

// TODO: Auto-generated Javadoc
/**
 * Common metadata for matched storable objects.
 */
public abstract class BaseMatchedStorableDTO extends BaseParsedAndCleanStorableDTO {

    /**
     * Reference to id of the associated parsed object.
     */
    private String parsedObjectId;

    /**
     * Reference to id of the associated clean object.
     */
    private String cleanObjectId;

    /**
     * Gets the parsed object id.
     *
     * @return the parsed object id
     */
    public final String getParsedObjectId() {
        return parsedObjectId;
    }

    /**
     * Sets the parsed object id.
     *
     * @param parsedObjectId
     *            the new parsed object id
     */
    public final void setParsedObjectId(final String parsedObjectId) {
        this.parsedObjectId = parsedObjectId;
    }

    /**
     * Gets the clean object id.
     *
     * @return the clean object id
     */
    public final String getCleanObjectId() {
        return cleanObjectId;
    }

    /**
     * Sets the clean object id.
     *
     * @param cleanObjectId
     *            the new clean object id
     */
    public final void setCleanObjectId(final String cleanObjectId) {
        this.cleanObjectId = cleanObjectId;
    }
}
