package eu.dl.worker.clean.plugin;

import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.utils.DocumentUtils;
import eu.dl.worker.utils.ArrayUtils;

/**
 * Plugin used to clean documents.
 *
 * @author Tomas Mrazek
 */
public class DocumentPlugin extends BaseDateTimePlugin<DocumentPlugin, ParsedTender, CleanTender> {
    /**
     * Number formats.
     */
    private final List<NumberFormat> numberFormats;
    /**
     * Document type mapping.
     */
    private final Map<Enum, List<String>> typeMapping;


    /**
     * Constructor with plugin configuration.
     *
     * @param numberFormat
     *      number format
     * @param formatter
     *      date and date time formatter
     * @param typeMapping
     *      document type mapping
     */
    public DocumentPlugin(final NumberFormat numberFormat, final DateTimeFormatter formatter,
        final Map<Enum, List<String>> typeMapping) {
        super(formatter);
        this.numberFormats = Arrays.asList(numberFormat);
        this.typeMapping = typeMapping;
    }

    /**
     * Plugin constructor with configuration.
     *
     * @param numberFormat
     *      number format
     * @param formatters
     *      list of date and datetime formatters
     * @param typeMapping
     *      document type mapping
     */
    public DocumentPlugin(final NumberFormat numberFormat, final List<DateTimeFormatter> formatters,
        final Map<Enum, List<String>> typeMapping) {
        super(formatters);
        this.numberFormats = Arrays.asList(numberFormat);
        this.typeMapping = typeMapping;
    }

    /**
     * Plugin constructor with configuration.
     *
     * @param numberFormats
     *      list fo number formats
     * @param formatters
     *      list of date and datetime formatters
     * @param typeMapping
     *      document type mapping
     */
    public DocumentPlugin(final List<NumberFormat> numberFormats, final List<DateTimeFormatter> formatters,
                          final Map<Enum, List<String>> typeMapping) {
        super(formatters);
        this.numberFormats = numberFormats;
        this.typeMapping = typeMapping;
    }

    /**
     * Cleans documents.
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
        if (parsedTender.getDocuments() != null) {
            logger.debug("Cleaning documents for parsed tender {} starts", parsedTender.getId());
            cleanTender.setDocuments(ArrayUtils.walk(parsedTender.getDocuments(),
                (parsedDocument) -> DocumentUtils.cleanDocument(parsedDocument, numberFormats, formatters,
                        typeMapping)));
            logger.debug("Cleaning documents for parsed tender {} finished", parsedTender.getId());
        }

        return cleanTender;
    }
}
