package eu.dl.worker.clean.utils;

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


/**
 * This class provide method for address cleaning.
 *
 * @author Tomas Mrazek
 */
public final class PriceUtils {

    private static final Logger logger = LoggerFactory.getLogger(PriceUtils.class.getName());

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
     *
     * @return cleaned price
     */
    public static Price cleanPrice(final ParsedPrice parsedPrice, final List<NumberFormat> numberFormat) {
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

        updateNetAmounts(price);

        // if there is a currency = EUR and value in netAmount available, we can set the netAmountInEur
        if (price.getCurrency() != null
                && price.getCurrency().getCurrencyCode().equals("EUR")
                &&  price.getNetAmount() != null) {
            price.setNetAmountEur(price.getNetAmount());
        }
        return price;
    }

    /**
     * Cleans the given parsed price.
     *
     * @param parsedPrice
     *          parsed price
     * @param numberFormat
     *          number format
     *
     * @return cleaned price
     */
    public static Price cleanPrice(final ParsedPrice parsedPrice, final NumberFormat numberFormat) {
        return cleanPrice(parsedPrice, Arrays.asList(numberFormat));
    }

    /**
     * Cleans currency.
     *
     * @see java.​util.Currency#getInstance(java.lang.String)
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
            .setUnitType((UnitType) CodeTableUtils.mapValue(parsedUnitPrice.getUnitNumber(), unitMapping))
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
     */
    public static void updateNetAmounts(final Price price) {
        if (price == null) {
            return;
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
