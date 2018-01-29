package eu.dl.worker.clean.utils;

import eu.dl.dataaccess.dto.generic.Corrigendum;
import eu.dl.dataaccess.dto.parsed.ParsedCorrigendum;
import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.List;

/**
 * This class provide method for corrigendum cleaning.
 *
 * @author Tomas Mrazek
 */
public final class CorrigendumUtils {

    /**
     * Utility classes should not have default constructor.
     */
    private CorrigendumUtils() {

    }

    /**
     * Cleans the given corrigendum.
     *
     * @param parsedCorrigendum
     *         parsed corrigendum
     * @param numberFormat
     *         list of number formats
     * @param formatters
     *         datetime formatters
     * @param country
     *          country
     * @return cleaned corrigendum
     */
    public static Corrigendum cleanCorrigendum(final ParsedCorrigendum parsedCorrigendum,
            final List<NumberFormat> numberFormat, final List<DateTimeFormatter> formatters, final String country) {
        if (parsedCorrigendum == null) {
            return null;
        }

        return new Corrigendum().setSectionNumber(StringUtils.cleanShortString(parsedCorrigendum.getSectionNumber()))
                .setLotNumber(NumberUtils.cleanInteger(parsedCorrigendum.getLotNumber(), numberFormat))
                .setPlaceOfModifiedText(StringUtils.cleanLongString(parsedCorrigendum.getPlaceOfModifiedText()))
                .setOriginal(StringUtils.cleanLongString(parsedCorrigendum.getOriginal()))
                .setReplacement(StringUtils.cleanLongString(parsedCorrigendum.getReplacement()))
                .setOriginalCpvs(CPVUtils.cleanCpvs(parsedCorrigendum.getOriginalCpvs()))
                .setReplacementCpvs(CPVUtils.cleanCpvs(parsedCorrigendum.getReplacementCpvs()))
                .setOriginalDate(DateUtils.cleanDateTime(parsedCorrigendum.getOriginalDate(), formatters))
                .setReplacementDate(DateUtils.cleanDate(parsedCorrigendum.getReplacementDate(), formatters))
                .setOriginalValue(PriceUtils.cleanPrice(parsedCorrigendum.getOriginalValue(), numberFormat, country))
                .setReplacementValue(PriceUtils.cleanPrice(parsedCorrigendum.getReplacementValue(), numberFormat,
                    country));
    }

    /**
     * Cleans the given corrigendum.
     *
     * @param parsedCorrigendum
     *         parsed corrigendum
     * @param numberFormat
     *         number format
     * @param formatter
     *         datetime formatter
     * @param country
     *          country
     * @return cleaned corrigendum
     */
    public static Corrigendum cleanCorrigendum(final ParsedCorrigendum parsedCorrigendum,
            final NumberFormat numberFormat, final List<DateTimeFormatter> formatter, final String country) {
        return cleanCorrigendum(parsedCorrigendum, Arrays.asList(numberFormat), formatter, country);
    }
}
