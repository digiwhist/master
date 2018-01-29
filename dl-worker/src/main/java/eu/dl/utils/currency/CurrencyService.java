package eu.dl.utils.currency;

import eu.dl.dataaccess.dto.ExchangeRates;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Currency;

/**
 * This service provides methods to work with currencies. For example conversion from/to EUR.
 *
 */
public interface CurrencyService {
    /**
     * Converts amount in currencyFrom to amount in currencyTo. There is used Exchange rate relevant for today.
     * 
     * @param currencyFrom currency in which the amount provided.
     * @param currencyTo currency in which should be the amount returned
     * @param amount amount
     * 
     * @return converted amount
     */
    BigDecimal convert(Currency currencyFrom, Currency currencyTo, BigDecimal amount);
    
    /**
     * Converts amount in currencyFrom to amount in currencyTo. There is used exchange rate 
     * relevant for the date provided.
     * 
     * @param currencyFrom currency in which the amount provided.
     * @param currencyTo currency in which should be the amount returned
     * @param amount amount
     * @param date exchange rate for the date is used
     * 
     * @return converted amount
     */
    BigDecimal convert(Currency currencyFrom, Currency currencyTo, BigDecimal amount, LocalDate date);
    
    /**
     * Gets the exchange rates for a current date. The inner implementation retrieves entry from 
     * a cache first, then from db. If nothing found locally, it tries to get data from remote API 
     * and store them. If stored into cache, the entry is valid for the whole lifetime
     * of the service unless cache cleaned explicitely.
     * 
     * @param date the date to be searched 
     * @return found result or null
     */
    ExchangeRates getExchangeRates(LocalDate date);

    /**
     * Updates the exchange rates for given date.
     *
     * @param date date for which exchange rates should be updated
     * @param exchangeRates updated exchange rates
     */
    void updateExchangeRates(LocalDate date, ExchangeRates exchangeRates);
}
