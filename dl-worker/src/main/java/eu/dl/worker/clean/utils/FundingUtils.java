package eu.dl.worker.clean.utils;

import java.text.NumberFormat;
import java.util.Arrays;
import java.util.List;

import eu.dl.dataaccess.dto.generic.Funding;
import eu.dl.dataaccess.dto.parsed.ParsedFunding;


/**
 * This class provide method for address cleaning.
 *
 * @author Tomas Mrazek
 */
public final class FundingUtils {

    /**
     * Utility classes should not have default constructor.
     */
    private FundingUtils() {

    }

    /**
     * Cleans the given parsed funding.
     *
     * @param parsedFunding
     *            parsed funding
     * @param numberFormats
     *            list of number formats
     * @param country
     *          country
     * @return cleaned funding
     */
    public static Funding cleanFunding(final ParsedFunding parsedFunding, final List<NumberFormat> numberFormats,
        final String country) {
        if (parsedFunding == null) {
            return null;
        }

        return new Funding()
            .setAmount(PriceUtils.cleanPrice(parsedFunding.getAmount(), numberFormats, country))
            .setIsEuFund(StringUtils.cleanBoolean(parsedFunding.getIsEuFund()))
            .setProgramme(StringUtils.cleanShortString(parsedFunding.getProgramme()))
            .setProportion(NumberUtils.cleanInteger(parsedFunding.getProportion(), numberFormats))
            .setSource(StringUtils.cleanShortString(parsedFunding.getSource()));
    }

    /**
     * Cleans the given parsed funding.
     *
     * @param parsedFunding
     *            parsed funding
     * @param numberFormat
     *            number format
     * @param country
     *          country
     * @return cleaned funding
     */
    public static Funding cleanFunding(final ParsedFunding parsedFunding, final NumberFormat numberFormat,
        final String country) {
        return cleanFunding(parsedFunding, Arrays.asList(numberFormat), country);
    }
}
