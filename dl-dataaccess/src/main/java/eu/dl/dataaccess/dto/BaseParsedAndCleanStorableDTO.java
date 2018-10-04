package eu.dl.dataaccess.dto;

import eu.dl.dataaccess.annotation.Transformable;

/**
 * Common metadata properties for parsed and clean objects.
 */
@Transformable
public abstract class BaseParsedAndCleanStorableDTO extends StorableDTO implements ParsedAndCleanStorableDTO {

    /**
     * Reference to id of the associated raw object.
     */
    private String rawObjectId;

    /* (non-Javadoc)
     * @see eu.dl.dataaccess.dto.ParsedAndCleanStorableDTO#getRawObjectId()
     */
    @Override
    public final String getRawObjectId() {
        return rawObjectId;
    }

    /* (non-Javadoc)
     * @see eu.dl.dataaccess.dto.ParsedAndCleanStorableDTO#setRawObjectId(java.lang.String)
     */
    @Override
    public final void setRawObjectId(final String rawObjectId) {
        this.rawObjectId = rawObjectId;
    }
}
