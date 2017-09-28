package eu.dl.worker.clean.plugin;

import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.utils.PublicationUtils;
import eu.dl.worker.utils.ArrayUtils;

/**
 * Plugin used to clean publications.
 *
 * @author Tomas Mrazek
 */
public class PublicationPlugin extends BaseDateTimePlugin<PublicationPlugin, ParsedTender, CleanTender> {
    private final List<NumberFormat> numberFormat;
    private final Map<Enum, List<String>> formTypeMapping;

    /**
     * Plugin constructor with configuration.
     *
     * @param numberFormat
     *      number format
     * @param formatter
     *      datetime formatter
     * @param formTypeMapping
     *      mapping for form type
     */
    public PublicationPlugin(final NumberFormat numberFormat, final DateTimeFormatter formatter,
        final Map<Enum, List<String>> formTypeMapping) {
        super(formatter);
        this.numberFormat = Arrays.asList(numberFormat);
        this.formTypeMapping = formTypeMapping;
    }

    /**
     * Plugin constructor with configuration.
     *
     * @param numberFormat
     *      number format
     * @param formatters
     *      list of datetime formatters
     * @param formTypeMapping
     *      mapping for form type
     */
    public PublicationPlugin(final NumberFormat numberFormat, final List<DateTimeFormatter> formatters,
        final Map<Enum, List<String>> formTypeMapping) {
        super(formatters);
        this.numberFormat = Arrays.asList(numberFormat);
        this.formTypeMapping = formTypeMapping;
    }

    /**
     * Plugin constructor with configuration.
     *
     * @param numberFormat
     *      list of number formats
     * @param formatters
     *      list of datetime formatters
     * @param formTypeMapping
     *      mapping for form type
     */
    public PublicationPlugin(final List<NumberFormat> numberFormat, final List<DateTimeFormatter> formatters,
        final Map<Enum, List<String>> formTypeMapping) {
        super(formatters);
        this.numberFormat = numberFormat;
        this.formTypeMapping = formTypeMapping;
    }

    /**
     * Cleans publications.
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
        if (parsedTender.getPublications() != null) {
            logger.debug("Cleaning publications for parsed tender {} starts", parsedTender.getId());
            cleanTender.setPublications(ArrayUtils.walk(parsedTender.getPublications(),
                (parsedPublication) -> PublicationUtils.cleanPublication(parsedPublication, numberFormat, formatters,
                    formTypeMapping)));
            logger.debug("Cleaning publications for parsed tender {} finished", parsedTender.getId());
        }

        return cleanTender;
    }
}
