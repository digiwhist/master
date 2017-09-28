package eu.dl.dataaccess.dto.parsed;

/**
 * Abstract price. Common price attributes.
 * @param <T>
 *         extending class (for fluent interface purposes)
 */
abstract class BaseParsedPrice<T extends BaseParsedPrice> {

    /**
     * Price without VAT.
     */
    private String netAmount;

    /**
     * Price including VAT.
     */
    private String amountWithVat;

    /**
     * VAT percentage.
     */
    private String vat;

    /**
     * ISO 4217 of used currency.
     */
    private String currency;

    /**
     * EUR equivalent of netAmount.
     */
    private String netAmountEur;

    /**
     * @return price without VAT
     */
    public String getNetAmount() {
        return netAmount;
    }

    /**
     * @param newNetAmount
     *            the netAmount to set
     *
     * @return instance of {@code T} class for chaining
     */
    public final T setNetAmount(final String newNetAmount) {
        this.netAmount = newNetAmount;
        return (T) this;
    }

    /**
     * @return the amountWithVat
     */
    public final String getAmountWithVat() {
        return amountWithVat;
    }

    /**
     * @param newAmountWithVat
     *            the amountWithVat to set
     *
     * @return instance of {@code T} class for chaining
     */
    public final T setAmountWithVat(final String newAmountWithVat) {
        this.amountWithVat = newAmountWithVat;
        return (T) this;
    }

    /**
     * @return the vat
     */
    public final String getVat() {
        return vat;
    }

    /**
     * @param newVat
     *            the vat to set
     *
     * @return instance of {@code T} class for chaining
     */
    public final T setVat(final String newVat) {
        this.vat = newVat;
        return (T) this;
    }

    /**
     * @return the currency
     */
    public final String getCurrency() {
        return currency;
    }

    /**
     * @param newCurrency
     *            the currency to set
     *
     * @return instance of {@code T} class for chaining
     */
    public final T setCurrency(final String newCurrency) {
        this.currency = newCurrency;
        return (T) this;
    }

    /**
     * @return the netAmountEur
     */
    public final String getNetAmountEur() {
        return netAmountEur;
    }

    /**
     * @param newNetAmountEur
     *            the netAmountEur to set
     *
     * @return instance of {@code T} class for chaining
     */
    public final T setNetAmountEur(final String newNetAmountEur) {
        this.netAmountEur = newNetAmountEur;
        return (T) this;
    }
}
