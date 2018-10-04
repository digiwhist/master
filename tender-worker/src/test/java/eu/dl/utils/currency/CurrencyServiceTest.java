package eu.dl.utils.currency;

import static org.junit.Assert.assertNull;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Currency;

import eu.dl.core.config.Config;

/**
 * Test for currency service.
 */
public final class CurrencyServiceTest {
    
    /**
     * Init.
     */
    public static void init() {
        Config.getInstance().addConfigFile("development");
    }
    
    /**
     * Test of null values.
     */
    public void nullValues() {
        CurrencyService currencyService = CurrencyServiceFactory.getCurrencyService();
        assertNull(currencyService.getExchangeRates(LocalDate.now().plusDays(50)));
    }
    
    /**
     * Test of currency conversion.
     */
    public void convert() {
        CurrencyService currencyService = CurrencyServiceFactory.getCurrencyService();
        currencyService.convert(Currency.getInstance("EUR"), Currency.getInstance("CZK"), 
                new BigDecimal(1000), LocalDate.now().minusDays(10));
        currencyService.convert(Currency.getInstance("CZK"), Currency.getInstance("EUR"), 
                new BigDecimal(1000), LocalDate.now().minusDays(10));
        currencyService.convert(Currency.getInstance("CZK"), Currency.getInstance("GBP"), 
                new BigDecimal(1000), LocalDate.now().minusDays(10));
        currencyService.convert(Currency.getInstance("CZK"), Currency.getInstance("GBP"), 
                new BigDecimal(1000), LocalDate.of(2007, 4, 6));
    }
}
