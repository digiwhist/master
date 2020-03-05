package eu.datlab.worker.in.clean;

import eu.datlab.worker.clean.BaseDatlabTenderCleaner;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.BuyerType;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.codetables.TenderSupplyType;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.AddressPlugin;
import eu.dl.worker.clean.plugin.BodyPlugin;
import eu.dl.worker.clean.plugin.CpvPlugin;
import eu.dl.worker.clean.plugin.DateTimePlugin;
import eu.dl.worker.clean.plugin.LotPlugin;
import eu.dl.worker.clean.plugin.PublicationPlugin;
import eu.dl.worker.clean.plugin.TenderProcedureTypePlugin;
import eu.dl.worker.clean.plugin.TenderSupplyTypePlugin;
import eu.dl.worker.clean.utils.CodeTableUtils;

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Tender cleaner for India.
 *
 * @author Tomas Mrazek
 */
public class CPPPTenderCleaner extends BaseDatlabTenderCleaner {
    private static final String VERSION = "1.0";

    private static final Locale LOCALE = new Locale("en", "EN");

    private static final NumberFormat NUMBER_FORMAT;
    static {
        DecimalFormatSymbols formatSymbols = new DecimalFormatSymbols(LOCALE);
        formatSymbols.setDecimalSeparator('.');
        formatSymbols.setGroupingSeparator(',');
        NUMBER_FORMAT = new DecimalFormat("#,##0.###", formatSymbols);
    }

    private static final DateTimeFormatter DATETIME_FORMAT = new DateTimeFormatterBuilder()
        .parseCaseInsensitive()
        .appendPattern("d-MMM-uuuu h:m a")
        .toFormatter(LOCALE);

    @SuppressWarnings("unchecked")
    @Override
    protected final void registerSpecificPlugins() {
        pluginRegistry
            .registerPlugin("publication", new PublicationPlugin(NUMBER_FORMAT, DATETIME_FORMAT, formTypeMapping()))
            .registerPlugin("address", new AddressPlugin())
            .registerPlugin("body", new BodyPlugin(buyerTypeMapping(), null, null))
            .registerPlugin("supplyType", new TenderSupplyTypePlugin(supplyTypeMapping()))
            .registerPlugin("procedureType", new TenderProcedureTypePlugin(procedureTypeMapping()))
            .registerPlugin("lots", new LotPlugin(NUMBER_FORMAT, DATETIME_FORMAT, new HashMap<>()))
            .registerPlugin("dateTime", new DateTimePlugin(DATETIME_FORMAT))
            .registerPlugin("cpv", new CpvPlugin());
    }

    /**
     * @return supply type mapping.
     */
    private Map<Enum, List<String>> supplyTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderSupplyType.SERVICES, Arrays.asList("Services", "Limited Tender(Serv)", "SERVICE", "Open Tender(Serv)",
            "Hiring of Vehicle", "support/maintenance services"));
        mapping.put(TenderSupplyType.SUPPLIES, Arrays.asList("Goods", "Goods Without TPS"));
        mapping.put(TenderSupplyType.WORKS, Arrays.asList("Works"));

        return mapping;
    }

    /**
     * @return procedure type mapping.
     */
    private Map<Enum, List<String>> procedureTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderProcedureType.OPEN, Arrays.asList("Open/Advertised", "Auction", "National Competitive Bid", "Open for H1 Price",
            "Open", "Open Tender", "OPEN TENDER-SRM", "e-Tender(ICB)", "National Competative Bid", "Auction", "Open Tender(Mtrl)",
            "International Competitive Bid", "Open Tender(Serv)"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITHOUT_PUBLICATION, Arrays.asList("Limited", "Open Limited", "Limited.",
            "LIMITED TENDER-SRM", "Limited Tender(Mtrl)", "Limited Open", "LT", "Closed Limited", "Limited Tender", "SINGLE TENDER-SRM",
            "LMTD", "Limited Tender(Serv)"));
        mapping.put(TenderProcedureType.OUTRIGHT_AWARD, Arrays.asList("Single", "Single Tender(Urg-M)", "Single Tender(Urg-S)",
            "Singl Tend-PAC/STD-M", "Single Tender(Oth-S)", "Single Tender", "Single Tender(Oth-M)", "Single Tender(PSU-M)",
            "single bid", "Single Tender(PSU-S)"));
        mapping.put(TenderProcedureType.CONCESSION, Arrays.asList("Expression of Interest"));
        mapping.put(TenderProcedureType.OTHER, Arrays.asList("Global Tenders", "Advertised", "Turnkey"));

        return mapping;
    }

    /**
     * @return buyer type mapping.
     */
    private Map<Enum, List<String>> buyerTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(BuyerType.PUBLIC_BODY, Arrays.asList("Autonomous Bodies"));
        mapping.put(BuyerType.OTHER, Arrays.asList("Public Sector Undertakings", "State Govt. and UT", "Public Sector Banks"));
        mapping.put(BuyerType.NATIONAL_AUTHORITY, Arrays.asList("Central Govt. Ministry/Department", "State Government"));

        return mapping;
    }

    /**
     * @return Form type mapping.
     */
    private Map<Enum, List<String>> formTypeMapping() {
        return CodeTableUtils.enumToMapping(PublicationFormType.class);
    }

    @Override
    public final String getVersion() {
        return VERSION;
    }

    @Override
    protected final CleanTender postProcessSourceSpecificRules(final ParsedTender parsedItem, final CleanTender cleanItem) {
        return cleanItem;
    }

    @Override
    protected final ParsedTender preProcessParsedItem(final ParsedTender parsedItem) {
        return parsedItem;
    }
}
