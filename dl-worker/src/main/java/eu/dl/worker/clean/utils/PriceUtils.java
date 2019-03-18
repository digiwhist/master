package eu.dl.worker.clean.utils;

import eu.dl.core.config.Config;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.NumberFormat;
import java.util.Arrays;
import java.util.Currency;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import eu.dl.dataaccess.dto.codetables.UnitType;
import eu.dl.dataaccess.dto.generic.Price;
import eu.dl.dataaccess.dto.generic.UnitPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedUnitPrice;
import eu.dl.utils.currency.BasicCurrencyService;
import eu.dl.utils.currency.UnconvertableException;
import org.bouncycastle.util.Strings;


/**
 * This class provide method for address cleaning.
 *
 * @author Tomas Mrazek
 */
public final class PriceUtils {

    private static final Logger logger = LoggerFactory.getLogger(PriceUtils.class.getName());

    private static final BigDecimal PRICE_MIN = BigDecimal.valueOf(100);

    private static final BigDecimal PRICE_MAX = BigDecimal.valueOf(1200000000);

    /**
     * Utility classes should not have default constructor.
     */
    private PriceUtils() {

    }

    /**
     * Cleans the given parsed price.
     *
     * @param parsedPrice
     *          parsed price
     * @param numberFormat
     *          list of number formats
     * @param country
     *          ISO code of country
     * @return cleaned price
     */
    public static Price cleanPrice(final ParsedPrice parsedPrice, final List<NumberFormat> numberFormat,
        final String country) {
        return cleanPrice(parsedPrice, numberFormat, country, true);
    }

    /**
     * Cleans the given parsed price.
     *
     * @param parsedPrice
     *          parsed price
     * @param numberFormat
     *          list of number formats
     * @param country
     *          ISO code of country
     * @param removeNonsensical
     *          remove nonsensical prices
     *
     * @return cleaned price
     */
    public static Price cleanPrice(final ParsedPrice parsedPrice, final List<NumberFormat> numberFormat,
                                   final String country, final boolean removeNonsensical) {
        if (parsedPrice == null) {
            return null;
        }

        Price price = new Price()
                .setAmountWithVat(NumberUtils.cleanBigDecimal(parsedPrice.getAmountWithVat(), numberFormat))
                .setCurrency(cleanPriceCurrency(parsedPrice.getCurrency()))
                .setMaxAmountWithVat(NumberUtils.cleanBigDecimal(parsedPrice.getMaxAmountWithVat(), numberFormat))
                .setMaxNetAmount(NumberUtils.cleanBigDecimal(parsedPrice.getMaxNetAmount(), numberFormat))
                .setMinAmountWithVat(NumberUtils.cleanBigDecimal(parsedPrice.getMinAmountWithVat(), numberFormat))
                .setMinNetAmount(NumberUtils.cleanBigDecimal(parsedPrice.getMinNetAmount(), numberFormat))
                .setNetAmount(NumberUtils.cleanBigDecimal(parsedPrice.getNetAmount(), numberFormat))
                .setNetAmountEur(NumberUtils.cleanBigDecimal(parsedPrice.getNetAmountEur(), numberFormat))
                .setVat(NumberUtils.cleanBigDecimal(removePercentFromVat(parsedPrice.getVat()), numberFormat));

        updateNetAmounts(price, country);

        Currency currency = price.getCurrency();
        if (currency == null && country != null) {
            String defaultCurrency = Config.getInstance().getParam("eu.datlab.worker."
                    + Strings.toLowerCase(country) + ".currency");

            if (defaultCurrency != null) {
                currency = Currency.getInstance(defaultCurrency);
                price.setCurrency(currency);
            }
        }

        // if there is a currency = EUR and value in netAmount available, we can set the netAmountInEur
        if (price.getCurrency() != null && price.getCurrency().getCurrencyCode().equals("EUR")
                && price.getNetAmount() != null) {
            price.setNetAmountEur(price.getNetAmount());
        }

        if (removeNonsensical) {
            price.setAmountWithVat(removeNonsensicalAmount(price.getAmountWithVat(), currency));
            price.setMaxAmountWithVat(removeNonsensicalAmount(price.getMaxAmountWithVat(), currency));
            price.setMinAmountWithVat(removeNonsensicalAmount(price.getMinAmountWithVat(), currency));
            price.setNetAmount(removeNonsensicalAmount(price.getNetAmount(), currency));
            price.setMaxNetAmount(removeNonsensicalAmount(price.getMaxNetAmount(), currency));
            price.setMinNetAmount(removeNonsensicalAmount(price.getMinNetAmount(), currency));
        }

        return price;
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
    public static BigDecimal removeNonsensicalAmount(final BigDecimal amount, final Currency currency,
        final BigDecimal min, final BigDecimal max) {
        if (currency == null) {
            return amount;
        }
        
        BigDecimal amountEUR = null;
        if (currency.getCurrencyCode().equals("EUR")) {
            amountEUR = amount;
        } else {
            try {
                BasicCurrencyService curr = new BasicCurrencyService();
                amountEUR = curr.convert(currency, Currency.getInstance("EUR"), amount);
            } catch (final UnconvertableException ex) {
                logger.warn("Unable to convert amount to EUR because of", ex);
            }
        }

        if (amountEUR != null
            && ((min != null && amountEUR.compareTo(min) < 0) || (max != null && amountEUR.compareTo(max) > 0))) {
            return null;
        }

        return amount;
    }

    /**
     * @param amount
     *      amount
     * @param currency
     *      currency
     * @return amount or null if the amount is out of range given by PriceUtils#PRICE_MIN and PriceUtils#PRICE_MAX
     */
    public static BigDecimal removeNonsensicalAmount(final BigDecimal amount, final Currency currency) {
        return removeNonsensicalAmount(amount, currency, PRICE_MIN, PRICE_MAX);
    }

    /**
     * Cleans the given parsed price.
     *
     * @param parsedPrice
     *          parsed price
     * @param numberFormat
     *          number format
     * @param country
     *          ISO code of country
     * @return cleaned price
     */
    public static Price cleanPrice(final ParsedPrice parsedPrice, final NumberFormat numberFormat,
        final String country) {
        return cleanPrice(parsedPrice, Arrays.asList(numberFormat), country);
    }

    /**
     * Cleans the given parsed price.
     *
     * @param parsedPrice
     *          parsed price
     * @param numberFormat
     *          number format
     * @param country
     *          ISO code of country
     * @param removeNonsensical
     *          remove nonsensical prices
     *
     * @return cleaned price
     */
    public static Price cleanPrice(final ParsedPrice parsedPrice, final NumberFormat numberFormat,
                                   final String country, final boolean removeNonsensical) {
        return cleanPrice(parsedPrice, Arrays.asList(numberFormat), country, removeNonsensical);
    }

    /**
     * Cleans currency.
     *
     * @see Currency#getInstance(java.lang.String)
     *
     * @param currency
     *      currency ISO 4217 code
     * @return currency
     */
    public static Currency cleanPriceCurrency(final String currency) {
        final String currencyForCleaning = StringUtils.prepareStringForCleaning(currency);
        if (currencyForCleaning == null || currencyForCleaning.isEmpty()) {
            return null;
        }

        //common typos repair
        String normCurrency = currencyForCleaning
                .replace("US$", "USD")
                .replace("dollars US", "USD")
                .replace("€", "EUR")
                .replace("francs suisses", "CHF")
                .replace("Hrvatska kuna", "HRK")
                .replace("forint", "HUF")
                .replace("yen", "JPY")
                .replace("Peso Chileno", "CLP")
                .replace("litai", "LTL");

        //processes only first three characters (ISO 4217 currency code consists of three alphabetical characters)
        if (normCurrency.length() > 3) {
            normCurrency = normCurrency.substring(0, 3);
        }

        // transform the currency to upper case, otherwise we fail for example for string "eur"
        normCurrency = normCurrency.toUpperCase();

        try {
            return Currency.getInstance(normCurrency);
        } catch (IllegalArgumentException e) {
            logger.error("Cleaning failed - currency code \"{}\" is not a supported ISO 4217 code", normCurrency);
            return null;
        }
    }

    /**
     * Cleans the given parsed unit price.
     *
     * @param parsedUnitPrice
     *          parsed unit price
     * @param numberFormat
     *          list of number formats
     * @param unitMapping
     *          mapping for price units
     * @return cleaned price
     */
    public static UnitPrice cleanUnitPrice(final ParsedUnitPrice parsedUnitPrice,
        final List<NumberFormat> numberFormat, final Map<Enum, List<String>> unitMapping) {
        if (parsedUnitPrice == null) {
            return null;
        }

        return new UnitPrice()
            .setAmountWithVat(NumberUtils.cleanBigDecimal(parsedUnitPrice.getAmountWithVat(), numberFormat))
            .setCurrency(cleanPriceCurrency(parsedUnitPrice.getCurrency()))
            .setNetAmount(NumberUtils.cleanBigDecimal(parsedUnitPrice.getNetAmount(), numberFormat))
            .setNetAmountEur(NumberUtils.cleanBigDecimal(parsedUnitPrice.getNetAmountEur(), numberFormat))
            .setUnitNumber(NumberUtils.cleanInteger(parsedUnitPrice.getUnitNumber(), numberFormat))
            .setUnitType((UnitType) CodeTableUtils.mapValue(parsedUnitPrice.getUnitType(), unitMapping))
            .setDescription(StringUtils.cleanLongString(parsedUnitPrice.getDescription()))
            .setVat(NumberUtils.cleanBigDecimal(removePercentFromVat(parsedUnitPrice.getVat()), numberFormat));
    }

    /**
     * Cleans the given parsed unit price.
     *
     * @param parsedUnitPrice
     *          parsed unit price
     * @param numberFormat
     *          number format
     * @param unitMapping
     *          mapping for price units
     * @return cleaned price
     */
    public static UnitPrice cleanUnitPrice(final ParsedUnitPrice parsedUnitPrice, final NumberFormat numberFormat,
        final Map<Enum, List<String>> unitMapping) {
        return cleanUnitPrice(parsedUnitPrice, Arrays.asList(numberFormat), unitMapping);
    }

    /**
     * The method removes percent sign from the end of VAT and returns it.
     *
     * @param vat
     *          string representation of VAT
     *
     * @return VAT without percent sign at the end
     */
    private static String removePercentFromVat(final String vat) {
        return vat != null && !vat.isEmpty() && vat.charAt(vat.length() - 1) == '%'
                ? vat.substring(0, vat.length() - 1)
                : vat;
    }

    /**
     * This methods sets net amounts (including min and max net amount) for price where possible.
     * Imagine situation, where there is vat and amountWithVat set but the value for netAmount is missing.
     * In such a case, we can easily calculate the netAmount value as
     *
     * netAmount = netAmountWithVat/((100+vat)/100)
     *
     * @param price price to be processed
     * @param country country
     */
    public static void updateNetAmounts(final Price price, final String country) {
        if (price == null) {
            return;
        }

        if (price.getVat() == null &&  country != null) {
            String vat = Config.getInstance().getParam("standardVat." + Strings.toUpperCase(country));
            if (vat != null) {
                price.setVat(new BigDecimal(vat));
            }
        }

        if (price.getVat() != null) {
            BigDecimal vatCoeficient = new BigDecimal(1);
            if (vatCoeficient.compareTo(new BigDecimal(0)) != 0) {
                vatCoeficient = price.getVat().add(new BigDecimal(100)).divide(
                        new BigDecimal(100), 10, RoundingMode.HALF_EVEN).abs();
            }

            if (price.getNetAmount() == null && price.getAmountWithVat() != null) {
                price.setNetAmount(price.getAmountWithVat().divide(vatCoeficient, 0, RoundingMode.HALF_EVEN));
            }

            if (price.getMinNetAmount() == null && price.getMinAmountWithVat() != null) {
                price.setMinNetAmount(price.getMinAmountWithVat().divide(vatCoeficient, 0, RoundingMode.HALF_EVEN));
            }

            if (price.getMaxNetAmount() == null && price.getMaxAmountWithVat() != null) {
                price.setMaxNetAmount(price.getMaxAmountWithVat().divide(vatCoeficient, 0, RoundingMode.HALF_EVEN));
            }
        }
    }
}
