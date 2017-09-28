package eu.dl.dataaccess.dto.matched;

/**
 * Unique bid id. Indetifies the bid by it's parent lot, tender id and bid id within the lot.
 */
public final class StructuredBidId extends BaseStructuredId<StructuredBidId> {
    private String bidId;

    /**
     * Gets bidId.
     *
     * @return value of bidId
     */
    public String getBidId() {
        return bidId;
    }

    /**
     * Sets bidId.
     *
     * @param bidId
     *         the bidId to set
     *
     * @return this instance for chaining
     */
    public StructuredBidId setBidId(final String bidId) {
        this.bidId = bidId;
        return this;
    }
}