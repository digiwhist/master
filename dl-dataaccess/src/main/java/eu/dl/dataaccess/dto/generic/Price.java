package eu.dl.dataaccess.dto.generic;

import com.fasterxml.jackson.annotation.JsonIgnore;
import eu.dl.dataaccess.annotation.Transformable;
import eu.dl.dataaccess.dto.clean.Validable;
import eu.dl.dataaccess.utils.ValidationUtils;

import java.math.BigDecimal;

/**
 * Price.
 */
@Transformable
public class Price extends BasePrice<Price> implements Validable {

    /**
     * Minimum reachable value of netAmount - typically given by range estimate
     * or spread of bids.
     */
    private BigDecimal minNetAmount;

    /**
     * Maximum reachable value of netAmount - typically given by range estimate
     * or spread of bids.
     */
    private BigDecimal maxNetAmount;

    /**
     * Minimum reachable value of amountWithVat.
     */
    private BigDecimal minAmountWithVat;

    /**
     * Maximum reachable value of amountWithVat.
     */
    private BigDecimal maxAmountWithVat;

    /**
     * @return minimum reachable value of netAmount
     */
    public final BigDecimal getMinNetAmount() {
        return minNetAmount;
    }

    /**
     * @param minNetAmount
     *         minimum reachable value of netAmount
     * @return this instance for chaining
     */
    public final Price setMinNetAmount(final BigDecimal minNetAmount) {
        this.minNetAmount = minNetAmount;
        return this;
    }

    /**
     * @return maximum reachable value of netAmount
     */
    public final BigDecimal getMaxNetAmount() {
        return maxNetAmount;
    }

    /**
     * @param maxNetAmount
     *         maximum reachable value of netAmount
     * @return this instance for chaining
     */
    public final Price setMaxNetAmount(final BigDecimal maxNetAmount) {
        this.maxNetAmount = maxNetAmount;
        return this;
    }

    /**
     * @return minimum reachable value of amountWithVat
     */
    public final BigDecimal getMinAmountWithVat() {
        return minAmountWithVat;
    }

    /**
     * @param minAmountWithVat
     *         minimum reachable value of amountWithVat
     * @return this instance for chaining
     */
    public final Price setMinAmountWithVat(final BigDecimal minAmountWithVat) {
        this.minAmountWithVat = minAmountWithVat;
        return this;
    }

    /**
     * @return maximum reachable value of amountWithVat
     */
    public final BigDecimal getMaxAmountWithVat() {
        return maxAmountWithVat;
    }

    /**
     * @param maxAmountWithVat
     *         maximum reachable value of amountWithVat
     * @return this instance for chaining
     */
    public final Price setMaxAmountWithVat(final BigDecimal maxAmountWithVat) {
        this.maxAmountWithVat = maxAmountWithVat;
        return this;
    }

    @Override
    @JsonIgnore
    public final Price getValid() {
        return ValidationUtils.getValid(this, amountWithVat, maxAmountWithVat, maxNetAmount, minAmountWithVat,
            minNetAmount, netAmount, netAmountEur, netAmountNational);
    }
}
