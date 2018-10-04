package eu.dl.worker.clean.utils;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.utils.RemoveNonsenseUtils;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Month;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.Arrays;
import java.util.List;
import java.util.function.BiFunction;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class holds methods handy for date and datetime cleaning.
 *
 * @author Kuba Krafka
 */
public final class DateUtils {

    private static final Logger logger = LoggerFactory.getLogger(DateUtils.class.getName());

    private static final LocalDate DATE_MIN = LocalDate.of(2000, Month.JANUARY, 1);
    private static final LocalDate DATE_MAX = LocalDate.of(2025, Month.JANUARY, 1);

    /**
     * Utility classes should not have default constructor.
     */
    private DateUtils() {

    }

    /**
     * Cleans the date based on the provided formatter.
     *
     * @param input
     *         value to be mapped
     * @param formatter
     *         will be used to parse date
     * @return parsed date
     */
    public static LocalDate cleanDate(final String input, final List<DateTimeFormatter> formatter) {
        final String inputForCleaning = StringUtils.prepareStringForCleaning(input);
        if (inputForCleaning == null || inputForCleaning.isEmpty()) {
            return null;
        }

        // if there is text ending with colon in the beginning of the input string, remove it
        String checkedInput = inputForCleaning.replaceAll("^\\D+:", "").trim();

        try {
            logger.debug("Cleaning date string \"{}\"", checkedInput);
            return parseDateTime(checkedInput, formatter, (dt, f) -> LocalDate.parse(dt, f));
        } catch (DateTimeParseException e) {
            logger.error("Cleaning failed - unable to parse \"{}\" into valid date.", checkedInput);
            return null;
        }
    }

    /**
     * Cleans the date based on the provided formatter.
     *
     * @param input
     *         value to be mapped
     * @param formatter
     *         will be used to parse date
     * @return parsed date
     */
    public static LocalDate cleanDate(final String input, final DateTimeFormatter formatter) {
        return cleanDate(input, Arrays.asList(formatter));
    }

    /**
     * Cleans the date time based on the provided formatter.
     *
     * @param input
     *         value to be mapped
     * @param formatter
     *         will be used to parse date
     * @return parsed date time
     */
    public static LocalDateTime cleanDateTime(final String input, final List<DateTimeFormatter> formatter) {
        final String inputForCleaning = StringUtils.prepareStringForCleaning(input);
        if (inputForCleaning == null || inputForCleaning.isEmpty()) {
            return null;
        }

        try {
            logger.debug("Cleaning datetime string \"{}\"", input);
            return parseDateTime(inputForCleaning, formatter, (dt, f) -> LocalDateTime.parse(dt, f));
        } catch (DateTimeParseException e) {
            logger.error("Cleaning failed - unable to parse \"{}\" into valid datetime.", input);
            return null;
        }
    }

    /**
     * Cleans the date time based on the provided formatter.
     *
     * @param input
     *         value to be mapped
     * @param formatter
     *         will be used to parse date
     * @return parsed date time
     */
    public static LocalDateTime cleanDateTime(final String input, final DateTimeFormatter formatter) {
        return cleanDateTime(input, Arrays.asList(formatter));
    }

    /**
     * Parses date(time) with help the given function {@code parsed}. This function accepts two parameters. The first
     * is date(time) string and second is formatter.
     *
     * @param <T>
     *         this instance should be LocalDate or LocalDateTime
     * @param dateTime
     *         date(time) string
     * @param formatters
     *         list of used formatters
     * @param parser
     *         function used for parsing
     * @return LocalDate or LocalDateTime
     * @throws DateTimeParseException
     *         in case that an error occurs during parsing
     */
    private static <T> T parseDateTime(final String dateTime, final List<DateTimeFormatter> formatters,
                                       final BiFunction<String, DateTimeFormatter, T> parser)
            throws DateTimeParseException {

        if (formatters.isEmpty()) {
            throw new UnrecoverableException("No formatter are defined.");
        }

        DateTimeParseException exception = null;
        for (DateTimeFormatter formatter : formatters) {
            try {
                return RemoveNonsenseUtils.removeNonsensicalDateTime(parser.apply(dateTime, formatter), DATE_MIN,
                    DATE_MAX);
            } catch (DateTimeParseException e) {
                exception = e;
            }
        }

        throw exception;
    }
}
