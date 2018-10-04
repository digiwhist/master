package eu.datlab.worker.eu.clean;


import eu.datlab.worker.clean.BaseDatlabTenderCleaner;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.BuyerActivityType;
import eu.dl.dataaccess.dto.codetables.BuyerType;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.SelectionMethod;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.codetables.TenderSupplyType;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.AddressPlugin;
import eu.dl.worker.clean.plugin.AwardCriteriaPlugin;
import eu.dl.worker.clean.plugin.BodyPlugin;
import eu.dl.worker.clean.plugin.DatePlugin;
import eu.dl.worker.clean.plugin.DateTimePlugin;
import eu.dl.worker.clean.plugin.FundingsPlugin;
import eu.dl.worker.clean.plugin.IntegerPlugin;
import eu.dl.worker.clean.plugin.LotPlugin;
import eu.dl.worker.clean.plugin.PricePlugin;
import eu.dl.worker.clean.plugin.PublicationPlugin;
import eu.dl.worker.clean.plugin.SelectionMethodPlugin;
import eu.dl.worker.clean.plugin.TenderProcedureTypePlugin;
import eu.dl.worker.clean.plugin.TenderSupplyTypePlugin;
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
 * Cleaner for the TED CSV tender.
 *
 * @author Tomas Mrazek
 */
public final class TedCSVTenderCleaner extends BaseDatlabTenderCleaner {
    private static final String VERSION = "1.0";

    private static final Locale LOCALE = Locale.US;

    private static final NumberFormat NUMBER_FORMAT;
    static {
        DecimalFormatSymbols formatSymbols = new DecimalFormatSymbols(LOCALE);
        formatSymbols.setDecimalSeparator('.');
        formatSymbols.setGroupingSeparator(',');
        NUMBER_FORMAT = new DecimalFormat("#,##0.###", formatSymbols);
    }

    private static final DateTimeFormatter DATETIME_FORMATTER = new DateTimeFormatterBuilder()
        .parseCaseInsensitive()
        .appendPattern("dd-MMM-yy[yy][ H:m]")        
        .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
        .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
        .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
        .toFormatter(LOCALE);

    private static final DateTimeFormatter PUBLICATION_YEAR_FORMATTER = new DateTimeFormatterBuilder()
        .appendPattern("yyyy")
        .parseDefaulting(ChronoField.MONTH_OF_YEAR, 1)
        .parseDefaulting(ChronoField.DAY_OF_MONTH, 1)
        .toFormatter(LOCALE);

    @Override
    protected void registerSpecificPlugins() {
        pluginRegistry
            .registerPlugin("integerPlugin", new IntegerPlugin(NUMBER_FORMAT))
            .registerPlugin("supplyType", new TenderSupplyTypePlugin(supplyTypeMapping()))
            .registerPlugin("procedureType", new TenderProcedureTypePlugin(procedureTypeMapping(),
                acceleratedProcedures()))
            .registerPlugin("date", new DatePlugin(DATETIME_FORMATTER))
            .registerPlugin("datetime", new DateTimePlugin(DATETIME_FORMATTER))
            .registerPlugin("bodies", new BodyPlugin(bodyTypeMapping(), bodyActivityMapping()))
            .registerPlugin("publications", new PublicationPlugin(NUMBER_FORMAT, DATETIME_FORMATTER, formTypeMapping())
                .addFormatter(PUBLICATION_YEAR_FORMATTER))            
            .registerPlugin("lots",
                new LotPlugin(NUMBER_FORMAT, DATETIME_FORMATTER, new HashMap<>()))
            .registerPlugin("prices", new PricePlugin(NUMBER_FORMAT))
            .registerPlugin("fundings", new FundingsPlugin(NUMBER_FORMAT))
            .registerPlugin("awardCriteria", new AwardCriteriaPlugin(NUMBER_FORMAT))
            .registerPlugin("address", new AddressPlugin())
            .registerPlugin("selectionMethod", new SelectionMethodPlugin(selectionMethodMapping()));
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
    private static Map<Enum, List<String>> supplyTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(TenderSupplyType.SERVICES, Arrays.asList("S"));
        mapping.put(TenderSupplyType.WORKS, Arrays.asList("W"));
        mapping.put(TenderSupplyType.SUPPLIES, Arrays.asList("U"));

        return mapping;
    }

    /**
     * @return procedure type mapping for cleaning process
     */
    private static Map<Enum, List<String>> procedureTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderProcedureType.OPEN, Arrays.asList("OPE"));
        mapping.put(TenderProcedureType.RESTRICTED, Arrays.asList("RES", "ARC"));
        mapping.put(TenderProcedureType.COMPETITIVE_DIALOG, Arrays.asList("COD"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITHOUT_PUBLICATION, Arrays.asList("NOC", "NOP"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITH_PUBLICATION, Arrays.asList("ACN", "NEC", "NEG", "NIC", "NIP"));
        mapping.put(TenderProcedureType.OUTRIGHT_AWARD, Arrays.asList("AWP"));
        
        return mapping;
    }

    /**
     * @return returns the list of accelerated procedures
     */
    private static List<String> acceleratedProcedures() {
        return Arrays.asList("ACN", "ARC");
    }

    /**
     * @return TED body activities mapping
     */
    private static Map<Enum, List<String>> bodyActivityMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        
        mapping.put(BuyerActivityType.DEFENCE, Arrays.asList("Defence"));
        mapping.put(BuyerActivityType.EDUCATION, Arrays.asList("Education"));
        mapping.put(BuyerActivityType.HEALTH, Arrays.asList("Health"));
        mapping.put(BuyerActivityType.RECREATION_CULTURE_AND_RELIGION,
            Arrays.asList("Recreation, culture and religion"));
        mapping.put(BuyerActivityType.ELECTRICITY, Arrays.asList("Electricity"));
        mapping.put(BuyerActivityType.ENVIRONMENT, Arrays.asList("Environment"));
        mapping.put(BuyerActivityType.OTHER, Arrays.asList("Other"));
        mapping.put(BuyerActivityType.RAILWAY, Arrays.asList("Railway services"));
        mapping.put(BuyerActivityType.AIRPORT, Arrays.asList("Airport-related activities"));
        mapping.put(BuyerActivityType.WATER, Arrays.asList("Water"));
        mapping.put(BuyerActivityType.ECONOMIC_AND_FINANCIAL_AFFAIRS,
            Arrays.asList("Economical and Financial Affairs", "Economical anf Financial Affairs"));
        mapping.put(BuyerActivityType.GENERAL_PUBLIC_SERVICES, Arrays.asList("General public\\services"));
        mapping.put(BuyerActivityType.COAL_AND_OTHER_EXTRACTION,
            Arrays.asList("Exploration and extraction of coal and other solid fuels"));
        mapping.put(BuyerActivityType.GAS_AND_OIL_EXTRACTION,
            Arrays.asList("Exploration and extraction of gas and oil"));
        mapping.put(BuyerActivityType.HOUSING_AND_COMMUNITY_AMENITIES,
            Arrays.asList("Housing and community amenities"));
        mapping.put(BuyerActivityType.SOCIAL_PROTECTION, Arrays.asList("Social protection"));
        mapping.put(BuyerActivityType.PORT, Arrays.asList("Port-related activities"));
        mapping.put(BuyerActivityType.POSTAL, Arrays.asList("Postal Services"));
        mapping.put(BuyerActivityType.GAS_AND_HEAT_PRODUCTION,
            Arrays.asList("Production, transport and distribution of gas and heat"));
        mapping.put(BuyerActivityType.PUBLIC_ORDER_AND_SAFETY, Arrays.asList("Public Order and Safety"));
        mapping.put(BuyerActivityType.URBAN_TRANSPORT,
            Arrays.asList("Urban railway, tramway, trolleybus or bus services"));

        return mapping;
    }

    /**
     * @return TED body type mapping
     */
    private static Map<Enum, List<String>> bodyTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        
        mapping.put(BuyerType.NATIONAL_AUTHORITY, Arrays.asList("1", "2"));
        mapping.put(BuyerType.REGIONAL_AUTHORITY, Arrays.asList("3"));
        mapping.put(BuyerType.EUROPEAN_AGENCY, Arrays.asList("5"));
        mapping.put(BuyerType.PUBLIC_BODY, Arrays.asList("6", "4"));
        mapping.put(BuyerType.OTHER, Arrays.asList("8", "9", "5A"));
        mapping.put(BuyerType.REGIONAL_AGENCY, Arrays.asList("R"));
        mapping.put(BuyerType.NATIONAL_AGENCY, Arrays.asList("N"));        
        mapping.put(null, Arrays.asList("Z"));

        return mapping;
    }

    /**
     * @return form type mapping
     */
    private Map<Enum, List<String>> formTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(PublicationFormType.CONTRACT_AWARD, Arrays.asList("3", "6"));
        mapping.put(PublicationFormType.CONTRACT_NOTICE, Arrays.asList("2", "5"));

        return mapping;
    }

    /**
     * @return selection method
     */
    private Map<Enum, List<String>> selectionMethodMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(SelectionMethod.LOWEST_PRICE, Arrays.asList("L"));
        mapping.put(SelectionMethod.MEAT, Arrays.asList("M"));

        return mapping;
    }

    @Override
    protected ParsedTender preProcessParsedItem(final ParsedTender parsedItem) {
        return parsedItem;
    }
}
