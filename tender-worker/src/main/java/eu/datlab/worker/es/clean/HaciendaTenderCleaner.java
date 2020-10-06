package eu.datlab.worker.es.clean;

import eu.datlab.worker.clean.BaseDatlabTenderCleaner;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.BuyerType;
import eu.dl.dataaccess.dto.codetables.CountryCode;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.SelectionMethod;
import eu.dl.dataaccess.dto.codetables.TenderLotStatus;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.codetables.TenderSize;
import eu.dl.dataaccess.dto.codetables.TenderSupplyType;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.AddressPlugin;
import eu.dl.worker.clean.plugin.AwardCriteriaPlugin;
import eu.dl.worker.clean.plugin.BodyPlugin;
import eu.dl.worker.clean.plugin.DatePlugin;
import eu.dl.worker.clean.plugin.DateTimePlugin;
import eu.dl.worker.clean.plugin.IntegerPlugin;
import eu.dl.worker.clean.plugin.LotPlugin;
import eu.dl.worker.clean.plugin.PricePlugin;
import eu.dl.worker.clean.plugin.PublicationPlugin;
import eu.dl.worker.clean.plugin.SelectionMethodPlugin;
import eu.dl.worker.clean.plugin.TenderProcedureTypePlugin;
import eu.dl.worker.clean.plugin.TenderSupplyTypePlugin;

import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.temporal.ChronoField;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

/**
 * Cleans Spanish tenders from Hacienda source.
 */
public final class HaciendaTenderCleaner extends BaseDatlabTenderCleaner {

    private static final String VERSION = "1.0";

    private static final NumberFormat NUMBER_FORMAT = NumberFormat.getInstance();
    private static final List<DateTimeFormatter> DATE_FORMATTERS = Arrays.asList(
            DateTimeFormatter.ISO_OFFSET_DATE,
            DateTimeFormatter.ofPattern("uuuu-MM-dd"));

    private static final List<DateTimeFormatter> DATETIME_FORMATTERS = Arrays.asList(
            DateTimeFormatter.ISO_OFFSET_DATE_TIME,
            new DateTimeFormatterBuilder()
                    .appendPattern("uuuu-MM-dd[ HH:mm:ss]")
                    //default values for time
                    .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
                    .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
                    .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
                    .toFormatter());

    @Override
    protected void registerSpecificPlugins() {
        Map<String, Map<Enum, List<String>>> lotMappings = new HashMap<>();
        lotMappings.put("countryMapping", countryMapping());
        lotMappings.put("statusMapping", statusMapping());

        pluginRegistry
                .registerPlugin("integerPlugin", new IntegerPlugin(NUMBER_FORMAT))
                .registerPlugin("procedureType",
                        new TenderProcedureTypePlugin(procedureTypeMapping()))
                .registerPlugin("supplyType", new TenderSupplyTypePlugin(supplyTypeMapping()))
                .registerPlugin("date", new DatePlugin<>(DATE_FORMATTERS))
                .registerPlugin("datetime", new DateTimePlugin<>(DATETIME_FORMATTERS))
                .registerPlugin("bodies", new BodyPlugin(typeMapping(), null, countryMapping()))
                .registerPlugin("publications",
                        new PublicationPlugin(NUMBER_FORMAT, DATE_FORMATTERS, formTypeMapping()))
                .registerPlugin("lots", new LotPlugin(NUMBER_FORMAT, DATE_FORMATTERS, lotMappings))
                .registerPlugin("prices", new PricePlugin(NUMBER_FORMAT))
                .registerPlugin("address", new AddressPlugin())
                .registerPlugin("selectionMethod", new SelectionMethodPlugin(null, selectionMethodRegexMapping(),
                        SelectionMethod.MEAT))
                .registerPlugin("awardCriteria", new AwardCriteriaPlugin(NUMBER_FORMAT));
    }

    /**
     * @return Selection method mapping
     */
    private Map<Enum, List<Pattern>> selectionMethodRegexMapping() {
        final Map<Enum, List<Pattern>> mapping = new HashMap<>();
        // (?i) makes the regex case insensitive
        mapping.put(SelectionMethod.LOWEST_PRICE, Arrays.asList(Pattern.compile("(?i).*precio.*")));
        return mapping;
    }

    /**
     * @return Supply type mapping
     */
    private Map<Enum, List<String>> supplyTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderSupplyType.SUPPLIES, Arrays.asList("1"));
        mapping.put(TenderSupplyType.SERVICES, Arrays.asList("2"));
        mapping.put(TenderSupplyType.WORKS, Arrays.asList("3"));
        mapping.put(TenderSupplyType.OTHER, Arrays.asList("50", "8", "7", "40", "32", "31", "22", "21"));

        return mapping;
    }

    /**
     * @return country mapping
     */
    private Map<Enum, List<String>> countryMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(CountryCode.AF, Arrays.asList("Afganistán"));
        mapping.put(CountryCode.BE, Arrays.asList("Bélgica"));
        mapping.put(CountryCode.ES, Arrays.asList("ES", "España", "ESPAÑA (PENI. BAL. Y CANARIAS)", "ESPANYA (PENI. BAL. I CANÀRIES)"));
        mapping.put(CountryCode.FR, Arrays.asList("Francia"));
        mapping.put(CountryCode.GB, Arrays.asList("Reino Unido"));
        mapping.put(CountryCode.DE, Arrays.asList("Alemania"));
        mapping.put(CountryCode.PT, Arrays.asList("Portugal"));
        mapping.put(CountryCode.US, Arrays.asList("Estados Unidos"));
        mapping.put(CountryCode.IT, Arrays.asList("Italia"));
        mapping.put(CountryCode.CU, Arrays.asList("Cuba"));

        return mapping;
    }

    /**
     * @return procedure type mapping
     */
    private Map<Enum, List<String>> procedureTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderProcedureType.OPEN, Arrays.asList("1"));
        mapping.put(TenderProcedureType.RESTRICTED, Arrays.asList("2"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITHOUT_PUBLICATION, Arrays.asList("3"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITH_PUBLICATION, Arrays.asList("4", "13"));
        mapping.put(TenderProcedureType.COMPETITIVE_DIALOG, Arrays.asList("5"));
        mapping.put(TenderProcedureType.MINITENDER, Arrays.asList("7"));
        mapping.put(TenderProcedureType.DESIGN_CONTEST, Arrays.asList("8"));
        mapping.put(TenderProcedureType.INOVATION_PARTNERSHIP, Arrays.asList("10"));
        mapping.put(TenderProcedureType.OTHER, Arrays.asList("100", "999", "6"));
        mapping.put(TenderProcedureType.APPROACHING_BIDDERS, Arrays.asList("9"));
        mapping.put(TenderProcedureType.DPS_PURCHASE, Arrays.asList("12"));

        return mapping;
    }

    /**
     * @return form type mapping
     */
    private Map<Enum, List<String>> formTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(PublicationFormType.CONTRACT_NOTICE, Arrays.asList("DOC_CN", "DOC_CD"));
        mapping.put(PublicationFormType.CONTRACT_AWARD,
                Arrays.asList("DOC_CAN_ADJ", "DOC_FORM", "DOC_CAN_DEF", "DOC_CAN_PROV"));
        mapping.put(PublicationFormType.CONTRACT_UPDATE, Arrays.asList("ANUL_CN", "ANUL_CAN_ADJ", "ANUL_FORM"));
        mapping.put(PublicationFormType.COMPILED, Arrays.asList("COMPILED"));
        mapping.put(PublicationFormType.PRIOR_INFORMATION_NOTICE, Arrays.asList("DOC_PIN", "DOC_ATPV"));
        mapping.put(PublicationFormType.CONTRACT_AMENDMENT, Arrays.asList("DOC_MOD"));
        mapping.put(PublicationFormType.CONTRACT_CANCELLATION, Arrays.asList("DESISTIMIENTO", "RENUNCIA"));
        mapping.put(PublicationFormType.OTHER, Arrays.asList("OTHER", "DOC_DESC"));

        return mapping;
    }

    /**
     * @return status mapping
     */
    private Map<Enum, List<String>> statusMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(TenderLotStatus.AWARDED, Arrays.asList("RES-1", "RES-2", "ADJ-1", "ADJ-2", "ADJ-8",
                "RES-8", "RES-9", "ADJ-9", "CREA-8", "EV-8"));
        mapping.put(TenderLotStatus.CANCELLED, Arrays.asList("ADJ-3", "RES-3", "ADJ-4", "RES-4", "RES-5", "RES-7",
                "ADJ-5", "ADJ-7"));
        mapping.put(TenderLotStatus.FINISHED, Arrays.asList("ANUL-3", "ANUL-4", "ANUL-8", "ANUL-9"));
        return mapping;
    }

    /**
     * @return body type mapping
     */
    private Map<Enum, List<String>> typeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(BuyerType.REGIONAL_AUTHORITY, Arrays.asList("2", "3"));
        mapping.put(BuyerType.NATIONAL_AUTHORITY, Arrays.asList("1"));
        mapping.put(BuyerType.PUBLIC_BODY, Arrays.asList("5"));
        mapping.put(BuyerType.NATIONAL_AGENCY, Arrays.asList("4"));

        return mapping;
    }

    @Override
    public String getVersion() {
        return VERSION;
    }

    @Override
    protected CleanTender postProcessSourceSpecificRules(final ParsedTender parsedItem, final CleanTender cleanItem) {
        if(cleanItem.getNationalProcedureType() != null && cleanItem.getNationalProcedureType().equals("6")) {
            cleanItem.setSize(TenderSize.SMALL_SCALE);
        }
        return cleanItem;
    }

    @Override
    protected ParsedTender preProcessParsedItem(final ParsedTender parsedItem) {
        return parsedItem;
    }
}
