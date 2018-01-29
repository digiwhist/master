package eu.dl.dataaccess.utils;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

/**
 * Class provides functions for nonsense value removing.
 */
public final class RemoveNonsenseUtils {
    
    
    /**
     * Suppress default constructor for noninstantiability.
     */
    private RemoveNonsenseUtils() {
    }
 
     /**
     * @param amount
     *      amount
     * @param currency
     *      currency
     * @param min
     *      minimal acepted value
     * @param max
     *      maximal accepted value
     * @return amount or null if the amount is out of range given by min and max
     */
//    public static BigDecimal removeNonsensicalAmount(final BigDecimal amount, final Currency currency,
//        final BigDecimal min, final BigDecimal max) {
//        if (currency == null) {
//            return amount;
//        }
//        
//        BigDecimal amountEUR;
//        if (currency.getCurrencyCode().equals("EUR")) {
//            amountEUR = amount;
//        } else {
//            BasicCurrencyService curr = new BasicCurrencyService();
//            amountEUR = curr.convert(currency, Currency.getInstance("EUR"), amount);
//        }
//
//        if (amountEUR != null
//            && ((min != null && amountEUR.compareTo(min) < 0) || (max != null && amountEUR.compareTo(max) > 0))) {
//            return null;
//        }
//
//        return amount;
//    }
    
    /**
     * @param <T>
     *      this instance should be LocalDate or LocalDateTime
     * @param datetime
     *      date(time)
     * @param min
     *      minimal accepted value
     * @param max
     *      maximal accepted value
     * @return datetime if the value is in range given by min and max, otherwise null
     */
    public static <T> T removeNonsensicalDateTime(final T datetime, final LocalDate min, final LocalDate max) {
        if (datetime == null) {
            return null;
        }

        LocalDate date = datetime instanceof LocalDateTime
            ? ((LocalDateTime) datetime).toLocalDate() : (LocalDate) datetime;

        if ((min != null && date.isBefore(min)) || (max != null && date.isAfter(max))) {
            return null;
        }

        return datetime;
    }

    /**
     * @param amount
     *      amount
     * @param min
     *      minimal acepted value
     * @param max
     *      maximal accepted value
     * @return amount or null if the amount is out of range given by min and max
     */
    public static BigDecimal removeNonsensicalAmount(final BigDecimal amount, final BigDecimal min,
        final BigDecimal max) {
        if (amount != null
            && ((min != null && amount.compareTo(min) < 0) || (max != null && amount.compareTo(max) > 0))) {
            return null;
        }

        return amount;
    }
}
