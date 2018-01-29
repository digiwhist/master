package eu.dl.worker.clean.utils;

import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.List;

import eu.dl.dataaccess.dto.generic.Payment;
import eu.dl.dataaccess.dto.parsed.ParsedPayment;


/**
 * This class provide method for payment cleaning.
 *
 * @author Tomas Mrazek
 */
public final class PaymentUtils {

    /**
     * Utility classes should not have default constructor.
     */
    private PaymentUtils() {

    }

    /**
     * Cleans the given payment.
     *
     * @param parsedPayment
     *          parsed payment
     * @param numberFormat
     *          list of number formats
     * @param formatter
     *          datetime formatter
     * @param country
     *          country
     * @return cleaned payment
     */
    public static Payment cleanPayment(final ParsedPayment parsedPayment, final List<NumberFormat> numberFormat,
        final List<DateTimeFormatter> formatter, final String country) {
        if (parsedPayment == null) {
            return null;
        }

        return new Payment()
            .setPaymentDate(DateUtils.cleanDate(parsedPayment.getPaymentDate(), formatter))
            .setPrice(PriceUtils.cleanPrice(parsedPayment.getPrice(), numberFormat, country));
    }

    /**
     * Cleans the given payment.
     *
     * @param parsedPayment
     *          parsed payment
     * @param numberFormat
     *          number format
     * @param formatter
     *          datetime formatter
     * @param country
     *          country
     * @return cleaned payment
     */
    public static Payment cleanPayment(final ParsedPayment parsedPayment, final NumberFormat numberFormat,
        final List<DateTimeFormatter> formatter, final String country) {
        return cleanPayment(parsedPayment, Arrays.asList(numberFormat), formatter, country);
    }
}
