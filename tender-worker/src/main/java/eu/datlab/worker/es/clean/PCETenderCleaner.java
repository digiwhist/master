package eu.datlab.worker.es.clean;

import eu.datlab.worker.clean.BaseDatlabTenderCleaner;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.CountryCode;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.TenderLotStatus;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.codetables.TenderSupplyType;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.AddressPlugin;
import eu.dl.worker.clean.plugin.AwardCriteriaPlugin;
import eu.dl.worker.clean.plugin.BodyPlugin;
import eu.dl.worker.clean.plugin.DatePlugin;
import eu.dl.worker.clean.plugin.DateTimePlugin;
import eu.dl.worker.clean.plugin.DocumentPlugin;
import eu.dl.worker.clean.plugin.FundingsPlugin;
import eu.dl.worker.clean.plugin.IntegerPlugin;
import eu.dl.worker.clean.plugin.LotPlugin;
import eu.dl.worker.clean.plugin.PricePlugin;
import eu.dl.worker.clean.plugin.PublicationPlugin;
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

/**
 * Created by michal on 22.1.17.
 */
public class PCETenderCleaner extends BaseDatlabTenderCleaner {
    private static final String VERSION = "2";

    private static final NumberFormat NUMBER_FORMAT = NumberFormat.getInstance();
    private static final List<DateTimeFormatter> DATE_FORMATTERS = Arrays.asList(
            DateTimeFormatter.ISO_OFFSET_DATE,
            DateTimeFormatter.ofPattern("uuuu-MM-dd"));

    // Examples of date time:
    // - 2012-02-24T14:00:00+01:00
    // - 2012-02-27
    private static final List<DateTimeFormatter> DATETIME_FORMATTERS = Arrays.asList(
            DateTimeFormatter.ISO_OFFSET_DATE_TIME,
            new DateTimeFormatterBuilder()
                    .appendPattern("uuuu-MM-dd")
                    //default values for time
                    .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
                    .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
                    .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
                    .toFormatter());

    @SuppressWarnings("unchecked")
    @Override
    protected final void registerSpecificPlugins() {
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
                .registerPlugin("bodies", new BodyPlugin(null, null, countryMapping()))
                .registerPlugin("publications",
                        new PublicationPlugin(NUMBER_FORMAT, DATE_FORMATTERS, formTypeMapping()))
                .registerPlugin("lots", new LotPlugin(NUMBER_FORMAT, DATE_FORMATTERS, lotMappings))
                .registerPlugin("prices", new PricePlugin(NUMBER_FORMAT))
                .registerPlugin("documents", new DocumentPlugin(NUMBER_FORMAT, DATETIME_FORMATTERS, null))
                .registerPlugin("address", new AddressPlugin())
                .registerPlugin("criteria", new AwardCriteriaPlugin(NUMBER_FORMAT))
                .registerPlugin("fundings", new FundingsPlugin(NUMBER_FORMAT))
                .registerPlugin("PCETSpecific", new PCETenderSpecificPlugin(depositsMapping()));
    }

    /**
     * @return Supply type mapping
     */
    private Map<Enum, List<String>> supplyTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderSupplyType.SUPPLIES, Arrays.asList("1", "4"));
        mapping.put(TenderSupplyType.SERVICES, Arrays.asList("2"));
        mapping.put(TenderSupplyType.WORKS, Arrays.asList("3"));
        mapping.put(TenderSupplyType.OTHER, Arrays.asList("50", "8", "7", "40", "31", "21", "0"));

        return mapping;
    }

    /**
     * @return deposit mapping
     */
    private Map<Enum, List<String>> depositsMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(PCETenderEnums.Deposits.Provisional, Arrays.asList("1"));
        mapping.put(PCETenderEnums.Deposits.Final, Arrays.asList("2"));
        mapping.put(PCETenderEnums.Deposits.Especial, Arrays.asList("3"));

        return mapping;
    }

    /**
     * @return country mapping
     */
    private Map<Enum, List<String>> countryMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(CountryCode.AF, Arrays.asList("Afganistán"));
        mapping.put(CountryCode.BE, Arrays.asList("Bélgica"));
        mapping.put(CountryCode.ES, Arrays.asList("España", "ESPAÑA (PENI. BAL. Y CANARIAS)", "ESPANYA (PENI. BAL. I CANÀRIES)"));
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
        mapping.put(TenderProcedureType.NEGOTIATED_WITH_PUBLICATION, Arrays.asList("4"));
        mapping.put(TenderProcedureType.COMPETITIVE_DIALOG, Arrays.asList("5"));
        mapping.put(TenderProcedureType.MINITENDER, Arrays.asList("6"));
        mapping.put(TenderProcedureType.OTHER, Arrays.asList("100", "7"));

        return mapping;
    }

    /**
     * @return form type mapping
     */
    private Map<Enum, List<String>> formTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(PublicationFormType.CONTRACT_NOTICE, Arrays.asList("ContractNotice", "CallForTenders"));
        mapping.put(PublicationFormType.CONTRACT_AWARD, Arrays.asList("ContractAwardNotice"));
        mapping.put(PublicationFormType.CONTRACT_UPDATE, Arrays.asList("ContractModificationNotice"));
        mapping.put(PublicationFormType.OTHER, Arrays.asList("HtmlDetail"));

        return mapping;
    }

    /**
     * @return status mapping
     */
    private Map<Enum, List<String>> statusMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderLotStatus.AWARDED, Arrays.asList("1", "2", "8", "9"));
        mapping.put(TenderLotStatus.CANCELLED, Arrays.asList("3", "4", "5", "6", "7"));

        return mapping;
    }

    @Override
    public final String getVersion() {
        return VERSION;
    }

    @Override
    protected final CleanTender postProcessSourceSpecificRules(final ParsedTender parsedItem,
                                                               final CleanTender cleanItem) {
        return cleanItem;
    }

    @Override
    protected final ParsedTender preProcessParsedItem(final ParsedTender parsedItem) {
        return parsedItem;
    }
}
