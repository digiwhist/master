package eu.dl.utils.currency;

/**
 * Factory for currency service.
 */
public class CurrencyServiceFactory {

    /**
     * Default constructor.
     */
    protected CurrencyServiceFactory() {
        // no public constructor available
    }

    /**
     * Creates and returns storage service.
     * 
     * @return currency service
     */
    public static final CurrencyService getCurrencyService() {
        return new BasicCurrencyService();
    }
}
