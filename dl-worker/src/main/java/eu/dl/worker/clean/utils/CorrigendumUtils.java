package eu.dl.worker.clean.utils;

import eu.dl.dataaccess.dto.generic.CPV;
import eu.dl.dataaccess.dto.generic.Corrigendum;
import eu.dl.dataaccess.dto.generic.Price;
import eu.dl.dataaccess.dto.parsed.ParsedCorrigendum;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;

import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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

        Corrigendum corrigendum = new Corrigendum().setSectionNumber(StringUtils.cleanShortString(parsedCorrigendum.getSectionNumber()))
                .setLotNumber(NumberUtils.cleanInteger(parsedCorrigendum.getLotNumber(), numberFormat))
                .setPlaceOfModifiedText(StringUtils.cleanLongString(parsedCorrigendum.getPlaceOfModifiedText()))
                .setOriginal(StringUtils.cleanLongString(parsedCorrigendum.getOriginal()))
                .setReplacement(StringUtils.cleanLongString(parsedCorrigendum.getReplacement()))
                .setOriginalCpvs(CPVUtils.cleanCpvs(parsedCorrigendum.getOriginalCpvs()))
                .setReplacementCpvs(CPVUtils.cleanCpvs(parsedCorrigendum.getReplacementCpvs()))
                .setOriginalDate(DateUtils.cleanDateTime(parsedCorrigendum.getOriginalDate(), formatters))
                .setReplacementDate(DateUtils.cleanDateTime(parsedCorrigendum.getReplacementDate(), formatters))
                .setOriginalValue(PriceUtils.cleanPrice(parsedCorrigendum.getOriginalValue(), numberFormat, country))
                .setReplacementValue(PriceUtils.cleanPrice(parsedCorrigendum.getReplacementValue(), numberFormat, country));

        if (corrigendum.getSectionNumber() != null && corrigendum.getReplacement() != null && corrigendum.getOriginalDate() == null
            && corrigendum.getReplacementDate() == null)
        {
            String section = corrigendum.getSectionNumber().replaceAll("^([IV]+(\\.\\d)+).*", "$1");

            switch (section) {
                // -- DATETIME --
                // bidDeadline
                case "III.3.1": case "IV.2.2": case "IV.3.4":
                // documentsDeadline
                case "IV.3.3":
                // any matching awardDecisionDate, contractSignatureDate, estimatedStartDate, estimatedCompletionDatea cancellationDate
                // on both tender and lot levels
                case "V.2.1": case "II.3":
                    corrigendum.setOriginalDate(DateUtils.cleanDateTime(corrigendum.getOriginal(), formatters));
                    corrigendum.setReplacementDate(DateUtils.cleanDateTime(corrigendum.getReplacement(), formatters));
                    break;

                // -- CPV --
                case "II.1.2": case "II.1.6": case "II.2.2":
                    corrigendum.setOriginalCpvs(parseCPVs(corrigendum.getOriginal()));
                    corrigendum.setReplacementCpvs(parseCPVs(corrigendum.getReplacement()));
                    break;

                // -- VALUE --
                // tender prices
                case "II.1.5": case "II.2.1": case "II.1.7":
                // lot prices
                case "II.2.6": case "V.2.4": case "V.4":
                    corrigendum.setOriginalValue(parsePrice(corrigendum.getOriginal(), numberFormat, country));
                    corrigendum.setReplacementValue(parsePrice(corrigendum.getReplacement(), numberFormat, country));
                    break;
                default:
                    break;
            }
        }

        return corrigendum;
    }

    /**
     * Attempts to parse list of CPVs from the given input string.
     *
     * @param input
     *      input string
     * @return non-empty list of CPVs or null
     */
    private static List<CPV> parseCPVs(final String input) {
        if (input == null) {
            return null;
        }

        List<CPV> cpvs = new ArrayList<>();
        Matcher m = Pattern.compile("\\d{8}([\\-\\. ]\\d)?").matcher(input);
        while (m.find()) {
            cpvs.add(new CPV().setCode(m.group()));
        }

        return cpvs.isEmpty() ? null : cpvs;
    }

    /**
     * Attempts to parse price from the given input string.
     *
     * @param input
     *      input string
     * @param numberFormat
     *      number format
     * @param country
     *      country
     * @return price or null
     */
    private static Price parsePrice(final String input, final List<NumberFormat> numberFormat, final String country) {
        if (input == null) {
            return null;
        }

        return PriceUtils.cleanPrice(new ParsedPrice().setNetAmount(input), numberFormat, country);
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
