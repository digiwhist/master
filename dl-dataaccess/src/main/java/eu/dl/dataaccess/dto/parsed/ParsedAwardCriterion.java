package eu.dl.dataaccess.dto.parsed;

// TODO: Auto-generated Javadoc
/**
 * Award criterion.
 */
public class ParsedAwardCriterion {
    /**
     * Name the criterion.
     */
    private String name;

    /**
     * Weight of the criterion.
     */
    private String weight;

    /**
     * Broader description of the criterion.
     */
    private String description;

    /**
     * The criterion is directly related to price (monetary measurable).
     */
    private String isPriceRelated;

    /**
     * Gets the name.
     *
     * @return the name
     */
    public final String getName() {
        return name;
    }

    /**
     * Sets the name.
     *
     * @param name
     *            the name
     * @return the parsed award criterion
     */
    public final ParsedAwardCriterion setName(final String name) {
        this.name = name;
        return this;
    }

    /**
     * Gets the weight.
     *
     * @return the weight
     */
    public final String getWeight() {
        return weight;
    }

    /**
     * Sets the weight.
     *
     * @param weight
     *            the weight
     * @return the parsed award criterion
     */
    public final ParsedAwardCriterion setWeight(final String weight) {
        this.weight = weight;
        return this;
    }

    /**
     * Gets the description.
     *
     * @return the description
     */
    public final String getDescription() {
        return description;
    }

    /**
     * Sets the description.
     *
     * @param description
     *            the description
     * @return the parsed award criterion
     */
    public final ParsedAwardCriterion setDescription(final String description) {
        this.description = description;
        return this;
    }

    /**
     * Gets the checks if is price related.
     *
     * @return the checks if is price related
     */
    public final String getIsPriceRelated() {
        return isPriceRelated;
    }

    /**
     * Sets the is price related.
     *
     * @param isPriceRelated
     *            the is price related
     * @return the parsed award criterion
     */
    public final ParsedAwardCriterion setIsPriceRelated(final String isPriceRelated) {
        this.isPriceRelated = isPriceRelated;
        return this;
    }
}
