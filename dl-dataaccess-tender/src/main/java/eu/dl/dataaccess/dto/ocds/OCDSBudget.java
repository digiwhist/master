package eu.dl.dataaccess.dto.ocds;

import eu.dl.dataaccess.annotation.Transformable;

/**
 * OCDS budget. This object doesn't cover full OCDS schema.
 * 
 * @see <a href="http://standard.open-contracting.org/1.1/en/schema/release/">OCDS Release Schema</a>
 */
@Transformable
public class OCDSBudget {
    private String id;

    private OCDSValue amount;

    private String description;

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
    public final OCDSBudget setId(final String id) {
        this.id = id;
        return this;
    }

    /**
     * @return amount
     */
    public final OCDSValue getAmount() {
        return amount;
    }

    /**
     * @param amount
     *      amount to be set
     * @return this instance for chaining
     */
    public final OCDSBudget setAmount(final OCDSValue amount) {
        this.amount = amount;
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
    public final OCDSBudget setDescription(final String description) {
        this.description = description;
        return this;
    }
}
