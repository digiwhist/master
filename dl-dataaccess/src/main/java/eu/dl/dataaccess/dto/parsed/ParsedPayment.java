package eu.dl.dataaccess.dto.parsed;

// TODO: Auto-generated Javadoc
/**
 * Payment. Information about actual transfer of money.
 */
public class ParsedPayment {
    /**
     * Date of actual payment.
     */
    private String paymentDate;

    /**
     * Price paid.
     */
    private ParsedPrice price;

    
    /**
     * Gets the payment date.
     *
     * @return the payment date
     */
    public final String getPaymentDate() {
        return paymentDate;
    }

    
    /**
     * Sets the payment date.
     *
     * @param newPaymentDate
     *            the new payment date
     * @return the parsed payment
     */
    public final ParsedPayment setPaymentDate(final String newPaymentDate) {
        this.paymentDate = newPaymentDate;
        return this;
    }

    
    /**
     * Gets the price.
     *
     * @return the price
     */
    public final ParsedPrice getPrice() {
        return price;
    }

    
    /**
     * Sets the price.
     *
     * @param newPrice
     *            the new price
     * @return the parsed payment
     */
    public final ParsedPayment setPrice(final ParsedPrice newPrice) {
        this.price = newPrice;
        return this;
    }
}
