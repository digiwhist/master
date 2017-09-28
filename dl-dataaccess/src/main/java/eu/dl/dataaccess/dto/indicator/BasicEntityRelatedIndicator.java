package eu.dl.dataaccess.dto.indicator;

import eu.dl.dataaccess.annotation.Transformable;

/**
 * Basic implementation of entiyt specific indicator.
 *
 */
@Transformable
public class BasicEntityRelatedIndicator extends BaseIndicatorDTO implements EntitySpecificIndicator {
    private String relatedEntityId;

    /**
     * @return the relatedEntityId
     */
    public final String getRelatedEntityId() {
        return relatedEntityId;
    }

    /**
     * @param relatedEntityId
     *            the relatedEntityId to set
     */
    public final void setRelatedEntityId(final String relatedEntityId) {
        this.relatedEntityId = relatedEntityId;
    }

}
