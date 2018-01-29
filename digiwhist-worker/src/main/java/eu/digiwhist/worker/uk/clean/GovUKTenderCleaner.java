package eu.digiwhist.worker.uk.clean;

import eu.digiwhist.worker.clean.BaseDigiwhistTenderCleaner;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.CountryCode;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.TenderLotStatus;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.codetables.TenderSupplyType;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.AddressPlugin;
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
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Tender cleaner for GOV UK.
 *
 * @author Michal Riha
 */
public class GovUKTenderCleaner extends BaseDigiwhistTenderCleaner {
    private static final String VERSION = "1.0";

    private static final NumberFormat NUMBER_FORMAT = NumberFormat.getInstance(new Locale("gb"));
    private static final List<DateTimeFormatter> DATETIME_FORMATTERS = Arrays.asList(
            DateTimeFormatter.ISO_LOCAL_DATE_TIME, DateTimeFormatter.ISO_OFFSET_DATE_TIME);

    @SuppressWarnings("unchecked")
    @Override
    protected final void registerSpecificPlugins() {
        Map<String, Map<Enum, List<String>>> lotMappings = new HashMap<>();
        lotMappings.put("countryMappings", countryMapping());
        lotMappings.put("statusMapping", statusMapping());

        pluginRegistry
                .registerPlugin("integerPlugin", new IntegerPlugin(NUMBER_FORMAT))
                .registerPlugin("supplyType", new TenderSupplyTypePlugin(supplyTypeMapping()))
                .registerPlugin("procedureType",
                        new TenderProcedureTypePlugin(procedureTypeMapping(), Arrays.asList("Accelerated",
                                "AcceleratedNegotiated")))
                .registerPlugin("date", new DatePlugin<>(DATETIME_FORMATTERS))
                .registerPlugin("datetime", new DateTimePlugin<>(DATETIME_FORMATTERS))
                .registerPlugin("bodies", new BodyPlugin(null, null, countryMapping()))
                .registerPlugin("publications",
                        new PublicationPlugin(NUMBER_FORMAT, DATETIME_FORMATTERS, getFormTypeMapping()))
                .registerPlugin("lots", new LotPlugin(NUMBER_FORMAT, DATETIME_FORMATTERS, lotMappings))
                .registerPlugin("prices", new PricePlugin(NUMBER_FORMAT))
                .registerPlugin("documents", new DocumentPlugin(NUMBER_FORMAT, DATETIME_FORMATTERS, null))
                .registerPlugin("address", new AddressPlugin())
                .registerPlugin("funding", new FundingsPlugin(NUMBER_FORMAT));
    }

    /**
     * Status mapping.
     *
     * @return status mapping
     */
    private Map<Enum, List<String>> statusMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(TenderLotStatus.AWARDED, Arrays.asList("Awarded"));
        mapping.put(TenderLotStatus.ANNOUNCED, Arrays.asList("Open", "Closed"));

        return mapping;
    }

    /**
     * @return country mapping
     */
    private Map<Enum, List<String>> countryMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(CountryCode.GB, Arrays.asList("United Kingdom", "England", "UK", "Wales", "Scotland", "Northern " +
                "Ireland"));
        mapping.put(CountryCode.AD, Arrays.asList("Andorra"));

        return mapping;
    }

    /**
     * @return procedure type mapping
     */
    private Map<Enum, List<String>> procedureTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(TenderProcedureType.OPEN, Arrays.asList("Open", "Accelerated"));
        mapping.put(TenderProcedureType.RESTRICTED, Arrays.asList("Restricted"));
        mapping.put(TenderProcedureType.NEGOTIATED, Arrays.asList("Negotiated", "AcceleratedNegotiated"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITHOUT_PUBLICATION, Arrays.asList("NegotiatedWithoutCompetition"));
        mapping.put(TenderProcedureType.OUTRIGHT_AWARD, Arrays.asList("AwardWithoutPriorPublication", "DirectAward"));
        mapping.put(TenderProcedureType.COMPETITIVE_DIALOG, Arrays.asList("CompetitiveDialog"));
        mapping.put(TenderProcedureType.OTHER, Arrays.asList("NotApplicable", "NotSpecified"));

        return mapping;
    }

    /**
     * @return supply type mapping
     */
    private Map<Enum, List<String>> supplyTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(TenderSupplyType.SERVICES, Arrays.asList("Services"));
        mapping.put(TenderSupplyType.WORKS, Arrays.asList("Works"));
        mapping.put(TenderSupplyType.SUPPLIES, Arrays.asList("Supplies", "Products"));        
        mapping.put(TenderSupplyType.OTHER, Arrays.asList("Other"));
        mapping.put(null, Arrays.asList("NotSpecified", "NotApplicable"));

        return mapping;
    }

    /**
     * @return form type mapping
     */
    private Map<Enum, List<String>> getFormTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(PublicationFormType.CONTRACT_NOTICE, Arrays.asList("open", "closed"));
        mapping.put(PublicationFormType.CONTRACT_AWARD, Arrays.asList("awarded"));

        return mapping;
    }

    @Override
    public final String getVersion() {
        return VERSION;
    }

    @Override
    protected final CleanTender postProcessSourceSpecificRules(final ParsedTender parsedTender,
                                                               final CleanTender cleanTender) {
        return cleanTender;
    }

    @Override
    protected final ParsedTender preProcessParsedItem(final ParsedTender parsedItem) {
        return parsedItem;
    }
}
