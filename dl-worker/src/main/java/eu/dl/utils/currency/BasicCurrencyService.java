package eu.dl.utils.currency;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import eu.dl.core.UnrecoverableException;
import eu.dl.core.config.Config;
import eu.dl.dataaccess.dao.ExchangeRatesDAO;
import eu.dl.dataaccess.dao.jdbc.JdbcExhangeRatesDAO;
import eu.dl.dataaccess.dto.ExchangeRates;
import eu.dl.worker.utils.ThreadUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.net.HttpURLConnection;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.Currency;
import java.util.HashMap;
import java.util.Map;

/**
 * Provides functionality related currency and its conversions.
 *
 */
public class BasicCurrencyService implements CurrencyService {
    
    private Logger logger;
    
    private ExchangeRatesDAO<ExchangeRates> dao;
    
    private Map<LocalDate, ExchangeRates> cache;
    
    private static final long RETENTION_PERIOD = 5;

    private static final String API_URL = "http://data.fixer.io/api/";
    
    /**
     * Creates currency service.
     * 
     */
    public BasicCurrencyService() {
        logger = LoggerFactory.getLogger(this.getClass());
        dao = new JdbcExhangeRatesDAO();
        cache = new HashMap<LocalDate, ExchangeRates>();
    }

    @Override
    public final BigDecimal convert(final Currency currencyFrom, final Currency currencyTo, final BigDecimal amount) {
        return convert(currencyFrom, currencyTo, amount, LocalDate.now());
    }

    @Override
    public final BigDecimal convert(final Currency currencyFrom, final Currency currencyTo, final BigDecimal amount, 
            final LocalDate date) {
        if (currencyFrom == null || currencyTo == null || amount == null) {
            logger.debug("Unable to convert amout {} from {} to {}, unsufficient data provided", 
                    amount, currencyFrom, currencyTo);
            return null;
        }
        
        // get exchange rates for date
        ExchangeRates exchangeRates = getExchangeRates(date);
        if (exchangeRates == null || exchangeRates.getRates() == null) {
            throw new UnconvertableException("Unable to convert currency, there are no exchange "
                    + "rates available for the desired date " + date);
        }
        
        // no convert needed, two same currencies provided
        if (currencyFrom.getCurrencyCode().equals(currencyTo.getCurrencyCode())) {
            logger.debug("{} {} converted to {}", amount, currencyFrom, currencyTo);
            return amount;
        }
        
        // currencyFrom is base
        if (currencyFrom.getCurrencyCode().equals(exchangeRates.getBase())) {
            logger.debug("currencyFrom is the base for exchange rates");
            BigDecimal exchangeRate = exchangeRates.getRates().get(currencyTo.getCurrencyCode());
            if (exchangeRate != null) {
                logger.debug("{} {} converted to {}", amount, currencyFrom, currencyTo);
                return amount.multiply(exchangeRate);
            } else {
                throw new UnconvertableException("Unable to convert currency, there is no exchange "
                        + "rates available for the desired currency " + currencyTo.getCurrencyCode());
            }
        }
        
        // currencyTo is base
        if (currencyTo.getCurrencyCode().equals(exchangeRates.getBase())) {
            BigDecimal exchangeRate = exchangeRates.getRates().get(currencyFrom.getCurrencyCode());
            if (exchangeRate != null) {
                logger.debug("{} {} converted to {}", amount, currencyFrom, currencyTo);
                return amount.divide(exchangeRate, 0, RoundingMode.HALF_EVEN);
            } else {
                throw new UnconvertableException("Unable to convert currency, there is no exchange "
                        + "rates available for the desired currency " + currencyFrom.getCurrencyCode());
            }
        }
        
        // convert through base currency
        BigDecimal exchangeRateFrom = exchangeRates.getRates().get(currencyFrom.getCurrencyCode());
        BigDecimal exchangeRateTo = exchangeRates.getRates().get(currencyTo.getCurrencyCode());
        if (exchangeRateFrom == null) {
            throw new UnconvertableException("Unable to convert currency, there is no exchange "
                    + "rates available for the desired currency " + currencyFrom.getCurrencyCode());
        }
        
        if (exchangeRateTo == null) {
            throw new UnconvertableException("Unable to convert currency, there is no exchange "
                    + "rates available for the desired currency " + currencyTo.getCurrencyCode());
        }
        
        return amount.divide(exchangeRateFrom, 0, RoundingMode.HALF_EVEN).multiply(exchangeRateTo);
    }

    /**
     * Gets the exchange rates for a current date. The inner implementation retrieves entry from 
     * a cache first, then from db. If nothing found locally, it tries to get data from remote API 
     * and store them. If stored into cache, the entry is valid for the whole lifetime
     * of the service unless cache cleaned explicitely.
     * 
     * @param date the date to be searched 
     * @return found result or null
     */
    public final ExchangeRates getExchangeRates(final LocalDate date) {
        // try cache first
        if (cache.containsKey(date)) {
            // rates for date found, return them
            logger.debug("Exchange rates for {} found in cache.", date);
            return cache.get(date);
        } else {
            // no cache entry, check db
            logger.debug("Exchange rates for {} not found in cache.", date);
            ExchangeRates exchangeRates = dao.getByDate(date);
            if (exchangeRates != null) {
                // found in db, return
                logger.debug("Exchange rates for {} found in db.", date);
                cache.put(date, exchangeRates);
                return exchangeRates;
            } else {
                // nothing found locally, download from remote API
                logger.debug("Exchange rates for {} not found in db.", date);
                exchangeRates = getExchangeRatesFromRemote(date);
                if (exchangeRates != null) {
                    dao.save(exchangeRates.setDate(date));
                    logger.debug("Exchange rates retrieved from API, storing to db with id {}.", exchangeRates.getId());
                }
                cache.put(date, exchangeRates);
                return exchangeRates;
            }
        }
    }

    /**
     * Retrieves exchange rates from remote API.
     * 
     * @param date date to retrieve exchange rates for
     * @return exchange rates or null
     */
    private ExchangeRates getExchangeRatesFromRemote(final LocalDate date) {
        try {
            ThreadUtils.humanize(2000);
            logger.debug("Querying exchange rates API for {}", date);
            URL url = new URL(getHistoricalRatesEndpointUrl(date));

            HttpURLConnection conn = (HttpURLConnection) url.openConnection();
            conn.setRequestMethod("GET");
            conn.setRequestProperty("Accept", "application/json");

            if (conn.getResponseCode() != 200) {
                throw new UnrecoverableException(
                        "Unable to retrieve data for " + date + ", http error code " + conn.getResponseCode());
            }

            BufferedReader br = new BufferedReader(new InputStreamReader(
                (conn.getInputStream())));

            String line;
            String response = "";
            while ((line = br.readLine()) != null) {
                response = response + line;
            }

            conn.disconnect();

            logger.debug("Retrieved connection rates from API");
            
            ObjectMapper mapper = new ObjectMapper();
            mapper.enable(DeserializationFeature.USE_BIG_DECIMAL_FOR_FLOATS);
            mapper.registerModule(new JavaTimeModule());
            mapper.setDateFormat(new SimpleDateFormat("yyyy-MM-dd"));
            mapper.disable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES);

            JsonNode error = mapper.readTree(response).findValue("error");
            // API return error response
            if (error != null) {
                logger.error("Unable to retrieve exchange rates for " + date + " because of API failed with error message "
                    + error.path("info").textValue());
                throw new UnrecoverableException("Unable to retrieve exchange rates for " + date);
            }

            ExchangeRates exchangeRates = mapper.readValue(response, ExchangeRates.class);
            
            // in some cases the api does not contain actual rates and returns a nonsense insted
            // we should ignore such results
            if (ChronoUnit.DAYS.between(exchangeRates.getDate(), date) > RETENTION_PERIOD) {
                logger.debug("API returned exchange rates for a to far date ({} instead of {}).", 
                        exchangeRates.getDate(), date);
                return null;
            }
            
            return exchangeRates;
        } catch (Exception e) {
            throw new UnrecoverableException("Unable to retrieve exchange rates for " + date, e);
        }
    }

    @Override
    public final void updateExchangeRates(final LocalDate date, final ExchangeRates exchangeRates) {
        cache.put(date, exchangeRates);
        dao.save(exchangeRates);
    }

    /**
     * @param date
     *      A date in the past for which historical rates are requested
     * @return endpoint url
     */
    private static String getHistoricalRatesEndpointUrl(final LocalDate date) {
        String accessKey = Config.getInstance().getParam("currency.api.access_key");
        return API_URL + date.format(DateTimeFormatter.ISO_LOCAL_DATE) + "?base=EUR&access_key=" + accessKey;
    }
}
