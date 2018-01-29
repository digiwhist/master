package eu.dl.worker.clean.plugin;

import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.List;

import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.utils.CleanUtils;
import eu.dl.worker.clean.utils.CorrigendumUtils;
import eu.dl.worker.utils.ArrayUtils;

/**
 * Plugin used to clean corrections.
 *
 * @author Tomas Mrazek
 */
public class CorrigendumPlugin extends BaseDateTimePlugin<CorrigendumPlugin, ParsedTender, CleanTender> {
    /**
     * Number formats.
     */
    private final List<NumberFormat> numberFormat;

    /**
     * Plugin constructor with configuration.
     *
     * @param numberFormat
     *      number format
     * @param formatter
     *      datetime formatter
     */
    public CorrigendumPlugin(final NumberFormat numberFormat, final DateTimeFormatter formatter) {
        super(formatter);
        this.numberFormat = Arrays.asList(numberFormat);
    }

    /**
     * Plugin constructor with configuration.
     *
     * @param numberFormat
     *      number format
      * @param formatters
     *       list of datetime formatters
     */
    public CorrigendumPlugin(final NumberFormat numberFormat, final List<DateTimeFormatter> formatters) {
        super(formatters);
        this.numberFormat = Arrays.asList(numberFormat);
    }

    /**
     * Plugin constructor with configuration.
     *
     * @param numberFormat
     *      list of number formats
      * @param formatters
     *       list of datetime formatters
     */
    public CorrigendumPlugin(final List<NumberFormat> numberFormat, final List<DateTimeFormatter> formatters) {
        super(formatters);
        this.numberFormat = numberFormat;
    }

    /**
     * Cleans corrections.
     *
     * @param parsedTender
     *            tender with source data
     * @param cleanTender
     *            tender with clean data
     *
     * @return tender with cleaned data
     */
    @Override
    public final CleanTender clean(final ParsedTender parsedTender, final CleanTender cleanTender) {
        if (parsedTender.getCorrections() != null) {
            logger.debug("Cleaning corrections in parsed tender {} starts", parsedTender.getId());
            cleanTender.setCorrections(ArrayUtils.walk(parsedTender.getCorrections(),
                (parsedCorrigendum) -> CorrigendumUtils.cleanCorrigendum(parsedCorrigendum, numberFormat, formatters,
                    CleanUtils.getParsedItemCountry(parsedTender))));
            logger.debug("Cleaning corrections in parsed tender {} finished", parsedTender.getId());
        }

        return cleanTender;
    }
}
