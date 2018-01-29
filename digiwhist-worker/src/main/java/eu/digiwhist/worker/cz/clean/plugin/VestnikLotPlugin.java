package eu.digiwhist.worker.cz.clean.plugin;

import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import eu.digiwhist.worker.cz.clean.utils.VestnikLotUtils;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.BaseDateTimePlugin;
import eu.dl.worker.clean.utils.CleanUtils;
import eu.dl.worker.utils.ArrayUtils;

/**
 * Vestnik specific plugin for cleaning lots.
 *
 * @author Tomas Mrazek
 */
public class VestnikLotPlugin extends BaseDateTimePlugin<VestnikLotPlugin, ParsedTender, CleanTender> {
    /**
     * Number formats.
     */
    private final List<NumberFormat> numberFormats;
    /**
     * Mapping.
     */
    private final Map<String, Map<Enum, List<String>>> mappings;
    
    /**
     * Plugin constructor with configuration.
     *
     * @param numberFormat
     *         number format
     * @param formatters
     *         list of datetime formatters
     * @param mappings
     *         mappings
     */
    public VestnikLotPlugin(final NumberFormat numberFormat, final List<DateTimeFormatter> formatters,
            final Map<String, Map<Enum, List<String>>> mappings) {
        super(formatters);

        this.numberFormats = Arrays.asList(numberFormat);
        this.mappings = mappings;
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
                (parsedLot) -> VestnikLotUtils.cleanLot(parsedLot, numberFormats, formatters, mappings,
                    CleanUtils.getParsedItemCountry(parsedTender))));
            logger.debug("Cleaning lots for parsed tender {} finished", parsedTender.getId());
        }

        return cleanTender;
    }
}
