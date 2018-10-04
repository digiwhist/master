package eu.datlab.worker.pl.clean;

import eu.datlab.worker.clean.BaseDatlabTenderCleaner;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.BuyerType;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.BodyPlugin;
import eu.dl.worker.clean.plugin.DatePlugin;
import eu.dl.worker.clean.plugin.DateTimePlugin;
import eu.dl.worker.clean.plugin.DocumentPlugin;
import eu.dl.worker.clean.plugin.IntegerPlugin;
import eu.dl.worker.clean.plugin.LotPlugin;
import eu.dl.worker.clean.plugin.PricePlugin;
import eu.dl.worker.clean.plugin.PublicationPlugin;
import eu.dl.worker.clean.plugin.TenderProcedureTypePlugin;
import eu.dl.worker.clean.plugin.TenderSupplyTypePlugin;

import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.format.SignStyle;
import java.time.temporal.ChronoField;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Cleaner for UZP in Json.
 *
 * @author Michal Riha
 */
public final class UZPTenderJsonCleaner extends BaseDatlabTenderCleaner {
    private static final String VERSION = "1.0";

    private static final NumberFormat NUMBER_FORMAT = NumberFormat.getInstance(new Locale("pl"));

    private static final List<DateTimeFormatter> DATE_FORMATTER;

    static {
        DATE_FORMATTER = new ArrayList<>();
        DATE_FORMATTER.add(DateTimeFormatter.ofPattern("yyyy-MM-dd"));
    }

    private static final DateTimeFormatter DATETIME_FORMATTER =
            new DateTimeFormatterBuilder().appendPattern("yyyy-MM-dd")
                    //optional time
                    .optionalStart()
                    .appendLiteral(" ")
                    .appendValue(ChronoField.HOUR_OF_DAY, 1, 2, SignStyle.NEVER)
                    .appendLiteral(":")
                    .appendValue(ChronoField.MINUTE_OF_HOUR, 2)
                    .optionalEnd()
                    //default values for time
                    .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
                    .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
                    .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
                    .toFormatter();

    @Override
    protected void registerSpecificPlugins() {
        pluginRegistry
                .registerPlugin("integerPlugin", new IntegerPlugin(NUMBER_FORMAT))
                .registerPlugin("supplyType", new TenderSupplyTypePlugin(null))
                .registerPlugin("procedureType", new TenderProcedureTypePlugin(null))
                .registerPlugin("date", new DatePlugin(DATE_FORMATTER))
                .registerPlugin("datetime", new DateTimePlugin(DATETIME_FORMATTER))
                .registerPlugin("bodies", new BodyPlugin(bodyTypeMapping(), null))
                .registerPlugin("publications",
                        new PublicationPlugin(NUMBER_FORMAT, DATE_FORMATTER, formTypeMapping()))
                .registerPlugin("lots", new LotPlugin(NUMBER_FORMAT, DATE_FORMATTER, new HashMap<>()))
                .registerPlugin("documents", new DocumentPlugin(NUMBER_FORMAT, DATE_FORMATTER, null))
                .registerPlugin("prices", new PricePlugin(NUMBER_FORMAT));
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

    @Override
    protected ParsedTender preProcessParsedItem(final ParsedTender parsedItem) {
        return parsedItem;
    }

    /**
     * @return form type mapping
     */
    private Map<Enum, List<String>> formTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(PublicationFormType.CONTRACT_NOTICE, Arrays.asList("ZP400", "ZP401", "ZP402", "ZP404",
                "ZP407", "ZP409"));
        mapping.put(PublicationFormType.CONTRACT_AWARD, Arrays.asList("ZP403", "ZP405", "ZP408"));
        mapping.put(PublicationFormType.CONTRACT_UPDATE, Arrays.asList("ZP406"));

        return mapping;
    }

    /**
     * @return body type mapping
     */
    private static Map<Enum, List<String>> bodyTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(BuyerType.PUBLIC_BODY, Arrays.asList("2"));
        mapping.put(BuyerType.NATIONAL_AUTHORITY, Arrays.asList("1"));
        mapping.put(BuyerType.REGIONAL_AUTHORITY, Arrays.asList("3"));
        mapping.put(BuyerType.OTHER, Arrays.asList("0"));

        return mapping;
    }
}
