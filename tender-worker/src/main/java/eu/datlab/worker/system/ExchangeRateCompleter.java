package eu.datlab.worker.system;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.ExchangeRates;
import eu.dl.utils.currency.CurrencyService;
import eu.dl.utils.currency.CurrencyServiceFactory;
import eu.dl.worker.BaseWorker;
import eu.dl.worker.Message;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.Map;

/**
 * Exchange rate downloader and calculator. Tries to download exchange rates if not already downloaded and calculate
 * missing rates from surrounding values.
 */
public final class ExchangeRateCompleter extends BaseWorker {
    private static final String INCOMING_EXCHANGE_NAME = "init";
    private static final String VERSION = "1.0";
    private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ISO_LOCAL_DATE;
    private static final LocalDate firstAvailableDate = LocalDate.of(1999, 1, 4);
    private static final String[] CURRENCIES_TO_CALCULATE = new String[] {
            "BGN",
            "CHF",
            "CZK",
            "GBP",
            "HRK",
            "HUF",
            "NOK",
            "PLN",
            "RON",
            "SEK"
    };

    @Override
    public void doWork(final Message message) {
        Map<String, ExchangeRates> exchangeRates = new HashMap<>();
        CurrencyService currencyService = CurrencyServiceFactory.getCurrencyService();

        for (LocalDate dateToDownload = firstAvailableDate; dateToDownload.isBefore(LocalDate.now()); dateToDownload =
        dateToDownload.plusDays(1)) {
            ExchangeRates exchangeRatesForDay = currencyService.getExchangeRates(dateToDownload);

            if (exchangeRatesForDay == null) {
                exchangeRates.put(dateToDownload.format(DATE_FORMATTER),
                        new ExchangeRates()
                        .setDate(dateToDownload)
                        .setBase("EUR"));
            } else {
                exchangeRates.put(dateToDownload.format(DATE_FORMATTER), exchangeRatesForDay);
            }
        }

//        for (LocalDate dateToFix = firstAvailableDate; dateToFix.isBefore(LocalDate.now()); dateToFix =
//                dateToFix.plusDays(1)) {
//            ExchangeRates ratesForDay = exchangeRates.get(dateToFix.format(DATE_FORMATTER));
//
//            for (String currency : CURRENCIES_TO_CALCULATE) {
//                BigDecimal newValue = calculateRateIfMissing(dateToFix, exchangeRates, currency);
//
//                if (newValue != null) {
//                    ratesForDay.getRates().put(currency, newValue);
//                }
//            }
//
//            currencyService.updateExchangeRates(dateToFix, ratesForDay);
//        }
    }

    @Override
    protected String getVersion() {
        return VERSION;
    }

    @Override
    protected String getIncomingExchangeName() {
        return INCOMING_EXCHANGE_NAME;
    }

    @Override
    protected String getIncomingQueueName() {
        return getIncomingQueueNameFromConfig();
    }

    @Override
    protected String getOutgoingExchangeName() {
        return null;
    }

    @Override
    protected void resend(final String version, final String dateFrom, final String dateTo) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    protected TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }

    /**
     * If rate for current day is missing calculate it from closest ones.
     *
     * @param dateToFix day to be fixed
     * @param exchangeRates all available rates
     * @param currency currency to be possibly fixed
     *
     * @return BigDecimal or null
     */
    private BigDecimal calculateRateIfMissing(final LocalDate dateToFix, final Map<String, ExchangeRates>
            exchangeRates, final String currency) {
        ExchangeRates ratesForDay = exchangeRates.get(dateToFix.format(DATE_FORMATTER));
        if (ratesForDay != null && ratesForDay.getRates().get(currency) == null) {
            BigDecimal previousRate = null;
            BigDecimal nextRate = null;

            ExchangeRates previousRates = exchangeRates.get(dateToFix.minusDays(1).format(DATE_FORMATTER));

            while (previousRates != null) {
                BigDecimal possibleRate = previousRates.getRates().get(currency);
                if (possibleRate != null) {
                    previousRate = possibleRate;
                    break;
                }
            }

            ExchangeRates nextRates = exchangeRates.get(dateToFix.minusDays(1).format(DATE_FORMATTER));

            while (nextRates != null) {
                BigDecimal possibleRate = nextRates.getRates().get(currency);
                if (possibleRate != null) {
                    nextRate = possibleRate;
                    break;
                }
            }

            if (previousRate == null || nextRate == null) {
                return null;
            } else {
                return previousRate.add(nextRate).divide(BigDecimal.valueOf(2));
            }
        }

        return null;
    }
}
