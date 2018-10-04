package eu.dl.dataaccess.dto.clean;

import eu.dl.dataaccess.dto.BaseParsedAndCleanStorableDTO;

/**
 * Common metadata for clean storable objects.
 */
public abstract class CleanStorableDTO extends BaseParsedAndCleanStorableDTO {

    /**
     * Reference to id of the associated parsed object.
     */
    private String parsedObjectId;

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
}
