package eu.datlab.worker.it.clean;

import eu.datlab.worker.clean.BaseDatlabTenderCleaner;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.TenderLotStatus;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.AddressPlugin;
import eu.dl.worker.clean.plugin.AwardCriteriaPlugin;
import eu.dl.worker.clean.plugin.BodyPlugin;
import eu.dl.worker.clean.plugin.DateTimePlugin;
import eu.dl.worker.clean.plugin.LotPlugin;
import eu.dl.worker.clean.plugin.PricePlugin;
import eu.dl.worker.clean.plugin.PublicationPlugin;

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.temporal.ChronoField;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Cleaner for the "Portale della trasparenza" tenders.
 *
 * @author Tomas Mrazek
 */
public final class ANACPDTTenderCleaner extends BaseDatlabTenderCleaner {
    private static final String VERSION = "1.0";

    private static final Locale LOCALE = new Locale("en");
    
    private static final NumberFormat NUMBER_FORMAT;
    static {
        DecimalFormatSymbols formatSymbols = new DecimalFormatSymbols(LOCALE);
        formatSymbols.setDecimalSeparator('.');
        formatSymbols.setGroupingSeparator(',');
        NUMBER_FORMAT = new DecimalFormat("#,##0.###", formatSymbols);
    }

    /**
     * DateTime format that sets time to 00:00:00 for the given date if needed.
     */
    private static final DateTimeFormatter DATETIME_FORMATTER = new DateTimeFormatterBuilder()
        .appendPattern("dd/MM/uuuu")
        //default values for time
        .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
        .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
        .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
        .toFormatter();
    
    @Override
    protected void registerSpecificPlugins() {
        Map<String, Map<Enum, List<String>>> lotMappings = new HashMap<>();
        lotMappings.put("statusMapping", statusMapping());

        pluginRegistry
            .registerPlugin("bodies", new BodyPlugin(null, null))
            .registerPlugin("publications",
                new PublicationPlugin(NUMBER_FORMAT, DATETIME_FORMATTER, null))
            .registerPlugin("lots",
                new LotPlugin(NUMBER_FORMAT, DATETIME_FORMATTER, lotMappings))
            .registerPlugin("prices", new PricePlugin(NUMBER_FORMAT))
            .registerPlugin("awardCriteria", new AwardCriteriaPlugin(NUMBER_FORMAT))
            .registerPlugin("address", new AddressPlugin())
            .registerPlugin("datetime", new DateTimePlugin(DATETIME_FORMATTER));
    }

    @Override
    public String getVersion() {
        return VERSION;
    }

    @Override
    protected CleanTender postProcessSourceSpecificRules(final ParsedTender parsedTender,
                                                         final CleanTender cleanTender) {
        return cleanTender;
    }

    /**
     * @return supply type mapping for cleaning process
     */
    private static Map<Enum, List<String>> statusMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(TenderLotStatus.FINISHED, Arrays.asList("Stato Avanzamento"));

        return mapping;
    }

    @Override
    protected ParsedTender preProcessParsedItem(final ParsedTender parsedItem) {
        return parsedItem;
    }
}
