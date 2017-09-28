package eu.dl.worker.clean.utils;

import java.math.BigDecimal;
import java.text.NumberFormat;
import java.text.ParseException;
import java.util.Arrays;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class holds methods handy for number cleaning.
 *
 * @author Kuba Krafka
 */
public final class NumberUtils {

    private static final Logger logger = LoggerFactory.getLogger(NumberUtils.class.getName());

    /**
     * Functional interface defines function for number parsing that throws java.text.ParseException.
     *
     * @param <S>
     *      number string
     * @param <F>
     *      NumberFormat
     * @param <N>
     *      Integer or BigDecimal number
     */
    @FunctionalInterface
    private interface NumberParserFunction<S, F, N> {
        /**
         * Method attempts parse number from the given string {@code s} using number format {@code f} and its method
         * parse.
         *
         * @see java.text.NumberFormat#parse(String)
         *
         * @param s
         *      number string
         * @param f
         *      number format
         * @return number
         * @throws ParseException
         *      in case that an error occurs during parsing
         */
        N apply(S s, F f) throws ParseException;
    }

    /**
     * Utility classes should not have default constructor.
     */
    private NumberUtils() {

    }

    /**
     * Parses provided string into an integer.
     *
     * @param input
     *            parsed string
     * @param format
     *            list of number formats     *
     * @return integer
     */
    public static Integer cleanInteger(final String input, final List<NumberFormat> format) {
        try {
            logger.debug("Cleaning integer string \"{}\"", input);
            return parseNumber(input, format, (i, f) -> f.parse(i).intValue());
        } catch (ParseException e) {
            logger.error("Cleaning failed - parsing string \"{}\" into integer failed because of {}", input, e);
            return null;
        }
    }

    /**
     * Parses provided string into an integer.
     *
     * @param input
     *            parsed string
     * @param format
     *            format
     *
     * @return integer
     */
    public static Integer cleanInteger(final String input, final NumberFormat format) {
        return cleanInteger(input, Arrays.asList(format));
    }

    /**
     * Parses provided string into an big decimal.
     *
     * @param input
     *            parsed string
     * @param format
     *            list of number formats
     *
     * @return big decimal
     */
    public static BigDecimal cleanBigDecimal(final String input, final List<NumberFormat> format) {
        try {
            logger.debug("Cleaning big decimal string \"{}\"", input);
            return parseNumber(input, format, (i, f) -> new BigDecimal(f.parse(i).toString()));
        } catch (ParseException e) {
            logger.error("Cleaning failed - parsing string \"{}\" into BigDecimal failed because of {}", input, e);
            return null;
        }
    }

    /**
     * Parses provided string into an big decimal.
     *
     * @param input
     *            parsed string
     * @param format
     *            number format
     *
     * @return big decimal
     */
    public static BigDecimal cleanBigDecimal(final String input, final NumberFormat format) {
        return cleanBigDecimal(input, Arrays.asList(format));
    }

    /**
     * Parses number with help the given function {@code parsed}. This function accepts two parameters. The first
     * is number string and second is number formats.
     *
     * @param <T>
     *         this instance should be Integer or BigDecimal
     * @param input
     *         number string
     * @param formats
     *         list of used number formats.
     * @param parser
     *         function used for number parsing.
     * @return number
     * @throws ParseException
     *         in case that an error occurs during parsing
     */
    private static <T> T parseNumber(final String input, final List<NumberFormat> formats,
        final NumberParserFunction<String, NumberFormat, T> parser) throws ParseException {

        final String inputForCleaning = removeNonDigitsBeginningAndEndLine(StringUtils.prepareStringForCleaning(input));
        if (inputForCleaning == null || inputForCleaning.isEmpty()) {
            return null;
        }

        ParseException exception = null;
        T best = null;
        for (NumberFormat format : formats) {
            try {
                final T current = parser.apply(inputForCleaning, format);
                /*
                 * the longest string from parsed number means better result because contains more information.
                 */
                if (best == null || best.toString().length() < current.toString().length()) {
                    best = current;
                }
            } catch (ParseException e) {
                exception = e;
            }
        }

        if (best == null && exception != null) {
            throw exception;
        }

        return best;
    }

    /**
     * Removes non digits on beginning and end of line.
     *
     * @param input input to shave
     * @return String without non digits on beginning and end of line
     */
    private static String removeNonDigitsBeginningAndEndLine(final String input) {
        if (input == null) {
            return null;
        }

        return input.replaceAll("^\\D*", "").replaceAll("\\D*$", "");
    }
}
