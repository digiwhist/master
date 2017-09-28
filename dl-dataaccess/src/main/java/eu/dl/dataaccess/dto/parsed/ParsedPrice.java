package eu.dl.dataaccess.dto.parsed;

/**
 * Price.
 */
public class ParsedPrice extends BaseParsedPrice<ParsedPrice> {

    /**
     * Minimum reachable value of netAmount - typically given by range estimate
     * or spread of bids.
     */
    private String minNetAmount;

    /**
     * Maximum reachable value of netAmount - typically given by range estimate
     * or spread of bids.
     */
    private String maxNetAmount;

    /**
     * Minimum reachable value of amountWithVat.
     */
    private String minAmountWithVat;

    /**
     * Maximum reachable value of amountWithVat.
     */
    private String maxAmountWithVat;


    /**
     * Gets the min net amount.
     *
     * @return the min net amount
     */
    public final String getMinNetAmount() {
        return minNetAmount;
    }

    /**
     * Sets the min net amount.
     *
     * @param minNetAmount
     *            the min net amount
     * @return the parsed price
     */
    public final ParsedPrice setMinNetAmount(final String minNetAmount) {
        this.minNetAmount = minNetAmount;
        return this;
    }

    /**
     * Gets the max net amount.
     *
     * @return the max net amount
     */
    public final String getMaxNetAmount() {
        return maxNetAmount;
    }

    /**
     * Sets the max net amount.
     *
     * @param maxNetAmount
     *            the max net amount
     * @return the parsed price
     */
    public final ParsedPrice setMaxNetAmount(final String maxNetAmount) {
        this.maxNetAmount = maxNetAmount;
        return this;
    }


    /**
     * Gets the min amount with vat.
     *
     * @return the min amount with vat
     */
    public final String getMinAmountWithVat() {
        return minAmountWithVat;
    }


    /**
     * Sets the min amount with vat.
     *
     * @param minAmountWithVat
     *            the min amount with vat
     * @return the parsed price
     */
    public final ParsedPrice setMinAmountWithVat(final String minAmountWithVat) {
        this.minAmountWithVat = minAmountWithVat;
        return this;
    }


    /**
     * Gets the max amount with vat.
     *
     * @return the max amount with vat
     */
    public final String getMaxAmountWithVat() {
        return maxAmountWithVat;
    }

    /**
     * Sets the max amount with vat.
     *
     * @param maxAmountWithVat
     *            the max amount with vat
     * @return the parsed price
     */
    public final ParsedPrice setMaxAmountWithVat(final String maxAmountWithVat) {
        this.maxAmountWithVat = maxAmountWithVat;
        return this;
    }
}
