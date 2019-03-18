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

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
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
public final class UZPJsonTenderCleaner extends BaseDatlabTenderCleaner {
    private static final String VERSION = "1.0";

    private static final NumberFormat NUMBER_FORMAT = NumberFormat.getInstance(new Locale("pl"));

    private static final List<NumberFormat> NUMBER_FORMATS = new ArrayList<>();

    static {
        DecimalFormatSymbols formatSymbols = new DecimalFormatSymbols(new Locale("pl"));
        formatSymbols.setDecimalSeparator(',');
        formatSymbols.setGroupingSeparator(' ');
        NUMBER_FORMATS.add(new DecimalFormat("#,##0.###", formatSymbols));
        NUMBER_FORMATS.add(NUMBER_FORMAT);
    }

    private static final List<DateTimeFormatter> DATE_FORMATTER;

    static {
        DATE_FORMATTER = new ArrayList<>();
        DATE_FORMATTER.add(DateTimeFormatter.ISO_LOCAL_DATE_TIME);
        DATE_FORMATTER.add(DateTimeFormatter.ofPattern("yyyy-MM-dd"));
    }

    private static final DateTimeFormatter DATETIME_FORMATTER = DateTimeFormatter.ISO_LOCAL_DATE_TIME;

    @Override
    protected void registerSpecificPlugins() {
        pluginRegistry
                .registerPlugin("integerPlugin", new IntegerPlugin(NUMBER_FORMATS))
                .registerPlugin("supplyType", new TenderSupplyTypePlugin(null))
                .registerPlugin("procedureType", new TenderProcedureTypePlugin(null))
                .registerPlugin("date", new DatePlugin(DATE_FORMATTER))
                .registerPlugin("datetime", new DateTimePlugin(DATETIME_FORMATTER))
                .registerPlugin("bodies", new BodyPlugin(bodyTypeMapping(), null))
                .registerPlugin("publications",
                        new PublicationPlugin(NUMBER_FORMATS, DATE_FORMATTER, formTypeMapping()))
                .registerPlugin("lots", new LotPlugin(NUMBER_FORMATS, DATE_FORMATTER, new HashMap<>()))
                .registerPlugin("documents", new DocumentPlugin(NUMBER_FORMATS, DATE_FORMATTER, null))
                .registerPlugin("prices", new PricePlugin(NUMBER_FORMATS));
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
