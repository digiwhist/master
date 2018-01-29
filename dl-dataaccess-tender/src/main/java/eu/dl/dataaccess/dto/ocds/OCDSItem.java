package eu.dl.dataaccess.dto.ocds;


import eu.dl.dataaccess.annotation.Transformable;

/**
 * OCDS item. This object doesn't cover full OCDS schema.
 * 
 * @see <a href="http://standard.open-contracting.org/1.1/en/schema/release/">OCDS Release Schema</a>
 */
@Transformable
public class OCDSItem {

    private String id;

    private String description;
    
    private String relatedLot;

    private OCDSItemClassification classification;

    /**
     * @return id
     */
    public final String getId() {
        return id;
    }

    /**
     * @param id
     *      id to be set
     * @return this instance for chaining
     */
    public final OCDSItem setId(final String id) {
        this.id = id;
        return this;
    }

    /**
     * @return description
     */
    public final String getDescription() {
        return description;
    }

    /**
     * @param description
     *      description to be set
     * @return this instance for chaining
     */
    public final OCDSItem setDescription(final String description) {
        this.description = description;
        return this;
    }

    /**
     * @return related lot
     */
    public final String getRelatedLot() {
        return relatedLot;
    }

    /**
     * @param relatedLot
     *      related lot to be set
     * @return this instance for chaining
     */
    public final OCDSItem setRelatedLot(final String relatedLot) {
        this.relatedLot = relatedLot;
        return this;
    }

    /**
     * @return classification
     */
    public final OCDSItemClassification getClassification() {
        return classification;
    }

    /**
     * @param classification
     *      classification to be set
     * @return this instance for chaining
     */
    public final OCDSItem setClassification(final OCDSItemClassification classification) {
        this.classification = classification;
        return this;
    }
}
