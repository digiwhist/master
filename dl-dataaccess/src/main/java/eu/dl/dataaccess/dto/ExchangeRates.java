package eu.dl.dataaccess.dto;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.HashMap;

/**
 * Contains exhange rates for a given date.
 *
 * @author Jakub Krafka
 */
public class ExchangeRates extends StorableDTO {
    /**
     * stores date for which are the rates valid.
     */
    private LocalDate date;
    
    /**
     * Base currency - the rates are relevant "against" this currency.
     */
    private String base;
    
    /**
     * Exchange rates.
     */
    private HashMap<String, BigDecimal> rates;

    /**
     * @return the date
     */
    public final LocalDate getDate() {
        return date;
    }

    /**
     * @param date the date to set
     * @return this for fluent interface
     */
    public final ExchangeRates setDate(final LocalDate date) {
        this.date = date;
        return this;
    }

    /**
     * @return the base
     */
    public final String getBase() {
        return base;
    }

    /**
     * @param base the base to set
     * @return this for fluent interface
     */
    public final ExchangeRates setBase(final String base) {
        this.base = base;
        return this;
    }

    /**
     * @return the rates
     */
    public final HashMap<String, BigDecimal> getRates() {
        return rates;
    }

    /**
     * @param rates the rates to set
     * @return this for fluent interface
     */
    public final ExchangeRates setRates(final HashMap<String, BigDecimal> rates) {
        this.rates = rates;
        return this;
    }
}
