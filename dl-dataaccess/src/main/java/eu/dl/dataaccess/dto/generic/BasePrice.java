package eu.dl.dataaccess.dto.generic;

import eu.dl.dataaccess.annotation.Transformable;
import eu.dl.dataaccess.dto.matched.MasterablePart;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Currency;

/**
 * Abstract price.
 * Common price attributes.
 *
 * @param <T>
 *         extending class (for fluent interface purposes)
 */
@Transformable
public abstract class BasePrice<T extends BasePrice> implements MasterablePart {
    /**
     * Price without VAT.
     */
    protected BigDecimal netAmount;

    /**
     * Price including VAT.
     */
    protected BigDecimal amountWithVat;

    /**
     * VAT percentage.
     */
    protected BigDecimal vat;

    /**
     * ISO 4217 of used currency.
     */
    protected Currency currency;

    /**
     * EUR equivalent of netAmount.
     */
    protected BigDecimal netAmountEur;
    
    /**
     * ISO 4217 of used currency. Holds national currency code.
     */
    protected Currency currencyNational;

    /**
     * National specific value. 
     */
    protected BigDecimal netAmountNational;

    /**
     * reliability for prices calculated in master record.
     */
    protected Float reliability;

    /**
     * Publication date.
     */
    protected LocalDate publicationDate;

    /**
     * Tender id.
     */
    protected String tenderId;

    /**
     * @return the netAmount
     */
    public final BigDecimal getNetAmount() {
        return netAmount;
    }

    /**
     * @param newNetAmount
     *      the netAmount to set
     *
     * @return instance of {@code T} class for chaining
     */
    public final T setNetAmount(final BigDecimal newNetAmount) {
        this.netAmount = newNetAmount;
        return (T) this;
    }

    /**
     * @return the amountWithVat
     */
    public final BigDecimal getAmountWithVat() {
        return amountWithVat;
    }

    /**
     * @param newAmountWithVat the amountWithVat to set
     *
     * @return instance of {@code T} class for chaining
     */
    public final T setAmountWithVat(final BigDecimal newAmountWithVat) {
        this.amountWithVat = newAmountWithVat;
        return (T) this;
    }

    /**
     * @return the vat
     */
    public final BigDecimal getVat() {
        return vat;
    }

    /**
     * @param newVat the vat to set
     *
     * @return instance of {@code T} class for chaining
     */
    public final T setVat(final BigDecimal newVat) {
        this.vat = newVat;
        return (T) this;
    }

    /**
     * @return the currency
     */
    public final Currency getCurrency() {
        return currency;
    }

    /**
     * @param newCurrency the currency to set
     *
     * @return instance of {@code T} class for chaining
     */
    public final T setCurrency(final Currency newCurrency) {
        this.currency = newCurrency;
        return (T) this;
    }
    
    /**
     * @return the currency
     */
    public final Currency getCurrencyNational() {
        return currencyNational;
    }

    /**
     * @param newCurrency the currency to set
     *
     * @return instance of {@code T} class for chaining
     */
    public final T setCurrencyNational(final Currency newCurrency) {
        this.currencyNational = newCurrency;
        return (T) this;
    }

    /**
     * @return the netAmountEur
     */
    public final BigDecimal getNetAmountEur() {
        return netAmountEur;
    }

    /**
     * @param newNetAmountEur the netAmountEur to set
     *
     * @return instance of {@code T} class for chaining
     */
    public final T setNetAmountEur(final BigDecimal newNetAmountEur) {
        this.netAmountEur = newNetAmountEur;
        return (T) this;
    }
    
    /**
     * @return the netAmountNational
     */
    public final BigDecimal getNetAmountNational() {
        return netAmountNational;
    }

    /**
     * @param newNetAmountEur the netAmountEur to set
     *
     * @return instance of {@code T} class for chaining
     */
    public final T setNetAmountNational(final BigDecimal newNetAmountEur) {
        this.netAmountNational = newNetAmountEur;
        return (T) this;
    }

    /**
     * @return reliability rating (calculated field in MASTER db)
     */
    public final Float getReliability() {
        return reliability;
    }

    /**
     * @param newReliability
     *         reliability rating (calculated field in MASTER db)
     * @return this instance for chaining
     */
    public final T setReliability(final Float newReliability) {
        this.reliability = newReliability;
        return (T) this;
    }

    @Override
    public final String getTenderId() {
        return tenderId;
    }

    /**
     * @param newTenderId
     *      tender id to be set
     * @return this instance for chaining
     */
    public final T setTenderId(final String newTenderId) {
        this.tenderId = newTenderId;
        return (T) this;
    }

    @Override
    public final LocalDate getPublicationDate() {
        return publicationDate;
    }

    /**
     * @param newPublicationDate
     *            the price publication date to set
     * @return this instance for chaining
     */
    public final T setPublicationDate(final LocalDate newPublicationDate) {
        this.publicationDate = newPublicationDate;
        return (T) this;
    }

    @Override
    public final LocalDateTime getCreatedRaw() {
        return null;
    }
}
