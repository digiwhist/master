package eu.dl.dataaccess.dto.indicator;

/**
 * Indicator relevant for entities.
 * 
 */
public interface EntitySpecificIndicator extends Indicator {
    /**
     * @return the relatedEntityId
     */
    String getRelatedEntityId();

    /**
     * @param relatedEntityId
     *            the relatedEntityId to set
     */
    void setRelatedEntityId(String relatedEntityId);
}
