package eu.dl.dataaccess.dto.generic;


import com.fasterxml.jackson.annotation.JsonIgnore;
import eu.dl.dataaccess.annotation.Transformable;
import eu.dl.dataaccess.dto.clean.Validable;
import eu.dl.dataaccess.utils.ValidationUtils;


/**
 * Award criterion.
 */
@Transformable
public class AwardCriterion implements Validable {

    /**
     * Name of criterion, PRICE if lowest price is used.
     */
    private String name;

    /**
     * Weight of criterion.
     */
    private Integer weight;

    /**
     * Broader description of the criterion.
     */
    private String description;

    /**
     * The criterion is directly related to price (monetary measurable).
     */
    private Boolean isPriceRelated;

    /**
     * @return the name
     */
    public final String getName() {
        return name;
    }

    /**
     * @param name
     *            the name to set
     * @return this instance for chaining
     */
    public final AwardCriterion setName(final String name) {
        this.name = name;
        return this;
    }

    /**
     * @return the weight
     */
    public final Integer getWeight() {
        return weight;
    }

    /**
     * @param weight
     *            the weight to set
     * @return this instance for chaining
     */
    public final AwardCriterion setWeight(final Integer weight) {
        this.weight = weight;
        return this;
    }

    /**
     * @return the description
     */
    public final String getDescription() {
        return description;
    }

    /**
     * @param description
     *            the description to set
     * @return this instance for chaining
     */
    public final AwardCriterion setDescription(final String description) {
        this.description = description;
        return this;
    }

    /**
     * @return the isPriceRelated
     */
    public final Boolean getIsPriceRelated() {
        return isPriceRelated;
    }

    /**
     * @param isPriceRelated
     *            the isPriceRelated to set
     * @return this instance for chaining
     */
    public final AwardCriterion setIsPriceRelated(final Boolean isPriceRelated) {
        this.isPriceRelated = isPriceRelated;
        return this;
    }

    @Override
    @JsonIgnore
    public final AwardCriterion getValid() {
        return ValidationUtils.getValid(this, name);
    }
}
