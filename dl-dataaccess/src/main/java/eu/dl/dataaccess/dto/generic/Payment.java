package eu.dl.dataaccess.dto.generic;

import com.fasterxml.jackson.annotation.JsonIgnore;
import eu.dl.dataaccess.annotation.Transformable;
import eu.dl.dataaccess.dto.clean.Validable;
import eu.dl.dataaccess.utils.ClassUtils;
import eu.dl.dataaccess.utils.ValidationUtils;

import java.time.LocalDate;

/**
 * Payment. Information about actual transfer of money.
 */
@Transformable
public class Payment implements Validable {
    /**
     * Date of actual payment.
     */
    private LocalDate paymentDate;

    /**
     * Price paid.
     */
    private Price price;

    /**
     * @return the paymentDate
     */
    public final LocalDate getPaymentDate() {
        return paymentDate;
    }

    /**
     * @param paymentDate
     *            the paymentDate to set
     * @return this instance for chaining
     */
    public final Payment setPaymentDate(final LocalDate paymentDate) {
        this.paymentDate = paymentDate;
        return this;
    }

    /**
     * @return the price
     */
    public final Price getPrice() {
        return price;
    }

    /**
     * @param price
     *            the price to set
     * @return this instance for chaining
     */
    public final Payment setPrice(final Price price) {
        this.price = price;
        return this;
    }

    @Override
    @JsonIgnore
    public final Payment getValid() {
        setPrice(ClassUtils.removeNonsenses(price));

        return ValidationUtils.getValid(this, price);
    }
}
