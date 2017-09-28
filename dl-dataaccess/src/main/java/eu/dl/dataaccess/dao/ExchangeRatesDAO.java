package eu.dl.dataaccess.dao;

import java.time.LocalDate;
import java.util.List;

/**
 * DAO for exchange rates.
 * 
 * @param <ExchangeRates>
 *            exchange rates 
 */
public interface ExchangeRatesDAO<ExchangeRates> {
    /**
     * Returns the object by given id.
     *
     * @param id
     *            id to be searched
     *
     * @return matched body with given id
     */
    ExchangeRates getById(String id);
    
    /**
     * Returns exchange rates for a given date.
     *
     * @param date
     *            get exchange rates for this date
     *
     * @return exchange rates
     */
    ExchangeRates getByDate(LocalDate date);
    
    /**
     * Returns all exchange rates.
     *
     * @return set of objects or empty list.
     */
    List<ExchangeRates> findAll();
    
    /**
     * Saves exchange rate object to db.
     *
     * @param exchangeRates object to be saved
     * 
     * @return id of saved of object
     */
    String save(ExchangeRates exchangeRates);
}
