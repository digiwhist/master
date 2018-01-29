package eu.dl.worker.clean.plugin;

import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.utils.CleanUtils;
import eu.dl.worker.clean.utils.LotUtils;
import eu.dl.worker.utils.ArrayUtils;

/**
 * Plugin used to clean lots.
 *
 * @author Tomas Mrazek
 */
public class LotPlugin extends BaseDateTimePlugin<LotPlugin, ParsedTender, CleanTender> {
    /**
     * Number formats.
     */
    private final List<NumberFormat> numberFormats;
    /**
     * Lot mappings.
     */
    private final Map<String, Map<Enum, List<String>>> lotMappings;

    /**
     * Plugin constructor with configuration.
     *
     * @param numberFormat
     *         number format
     * @param formatter
     *         datetime formatter
     * @param lotMappings
     *         lot mappings
     */
    public LotPlugin(final NumberFormat numberFormat, final DateTimeFormatter formatter,
                     final Map<String, Map<Enum, List<String>>> lotMappings) {
        super(formatter);
        this.numberFormats = Arrays.asList(numberFormat);
        this.lotMappings = lotMappings;
    }

    /**
     * Plugin constructor with configuration.
     *
     * @param numberFormat
     *         number format
     * @param formatters
     *         list of datetime formatters
     * @param lotMappings
     *         lot mappings
     */
    public LotPlugin(final NumberFormat numberFormat, final List<DateTimeFormatter> formatters,
                     final Map<String, Map<Enum, List<String>>> lotMappings) {
        super(formatters);
        this.numberFormats = Arrays.asList(numberFormat);
        this.lotMappings = lotMappings;
    }

    /**
     * Plugin constructor with configuration.
     *
     * @param numberFormats
     *         list of number formats
     * @param formatters
     *         list of datetime formatters
     * @param lotMappings
     *         lot mappings
     */
    public LotPlugin(final List<NumberFormat> numberFormats, final List<DateTimeFormatter> formatters,
                     final Map<String, Map<Enum, List<String>>> lotMappings) {
        super(formatters);
        this.numberFormats = numberFormats;
        this.lotMappings = lotMappings;
    }

    /**
     * Cleans lots.
     *
     * @param parsedTender
     *         tender with source data
     * @param cleanTender
     *         tender with clean data
     *
     * @return tender with cleaned data
     */
    @Override
    public final CleanTender clean(final ParsedTender parsedTender, final CleanTender cleanTender) {
        if (parsedTender.getLots() != null) {
            logger.debug("Cleaning lots for parsed tender {} starts", parsedTender.getId());
            cleanTender.setLots(ArrayUtils.walk(parsedTender.getLots(),
                    (parsedLot) -> LotUtils.cleanLot(parsedLot, numberFormats, formatters, lotMappings,
                        CleanUtils.getParsedItemCountry(parsedTender))));
            logger.debug("Cleaning lots for parsed tender {} finished", parsedTender.getId());
        }

        return cleanTender;
    }
}
