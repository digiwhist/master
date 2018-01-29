package eu.dl.dataaccess.dto.ocds;


import eu.dl.dataaccess.annotation.Transformable;
import java.math.BigDecimal;
import java.util.Currency;

/**
 * OCDS value. This object doesn't cover full OCDS schema.
 * 
 * @see <a href="http://standard.open-contracting.org/1.1/en/schema/release/">OCDS Release Schema</a>
 */
@Transformable
public class OCDSValue {

    private BigDecimal amount;

    /**
     * The currency in 3-letter ISO 4217 format.
     */
    private Currency currency;

    /**
     * @return amount
     */
    public final BigDecimal getAmount() {
        return amount;
    }

    /**
     * @param amount
     *      amount to be set
     * @return this instance for chaining
     */
    public final OCDSValue setAmount(final BigDecimal amount) {
        this.amount = amount;
        return this;
    }

    /**
     * @return currency
     */
    public final Currency getCurrency() {
        return currency;
    }

    /**
     * @param currency
     *      currency to be set
     * @return this instance for chaining
     */
    public final OCDSValue setCurrency(final Currency currency) {
        this.currency = currency;
        return this;
    }
}
