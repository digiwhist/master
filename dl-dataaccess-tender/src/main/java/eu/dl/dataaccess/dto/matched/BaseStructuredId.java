package eu.dl.dataaccess.dto.matched;

/**
 * Base Structured Id.
 * Common structured id attributes for lots and bids.
 *
 * @param <T>
 *         extending class (for fluent interface purposes)
 */
public abstract class BaseStructuredId<T extends BaseStructuredId> {
    private String tenderId;
    private String lotId;

    /**
     * Gets tenderId.
     *
     * @return value of tenderId
     */
    public final String getTenderId() {
        return tenderId;
    }

    /**
     * Sets tenderId.
     *
     * @param newTenderId
     *         the tenderId to set
     *
     * @return this instance for chaining
     */
    public final T setTenderId(final String newTenderId) {
        this.tenderId = newTenderId;
        return (T) this;
    }

    /**
     * Gets lotId.
     *
     * @return value of lotId
     */
    public final String getLotId() {
        return lotId;
    }

    /**
     * Sets lotId.
     *
     * @param newLotId
     *         the lotId to set
     *
     * @return this instance for chaining
     */
    public final T setLotId(final String newLotId) {
        this.lotId = newLotId;
        return (T) this;
    }
}
