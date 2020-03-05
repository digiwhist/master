package eu.datlab.worker.no.clean;

import eu.datlab.worker.clean.BaseDatlabTenderCleaner;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.BuyerType;
import eu.dl.dataaccess.dto.codetables.CountryCode;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.SelectionMethod;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.codetables.TenderSupplyType;
import eu.dl.dataaccess.dto.codetables.BuyerActivityType;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.AddressPlugin;
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
import eu.dl.worker.clean.plugin.AwardCriteriaPlugin;

import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.temporal.ChronoField;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Created by michalriha on 03/05/2017.
 */
public class DOFFINTenderCleaner extends BaseDatlabTenderCleaner {
    private static final String VERSION = "1.0";

    private static final NumberFormat NUMBER_FORMAT = NumberFormat.getInstance();

    private static final List<DateTimeFormatter> DATE_FORMATTERS = Arrays.asList(
            DateTimeFormatter.ofPattern("uuuu.M.d[.]"),
            DateTimeFormatter.ofPattern("uuuu-M-d"),
            DateTimeFormatter.ofPattern("d-M-uuuu"));

    private static final List<DateTimeFormatter> DATETIME_FORMATTERS = Arrays.asList(
        DateTimeFormatter.ISO_LOCAL_DATE_TIME,
        DateTimeFormatter.ISO_OFFSET_DATE_TIME,
        new DateTimeFormatterBuilder().appendPattern("uuuu.M.d[.][ H:m]")
            .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
            .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
            .toFormatter(),
        new DateTimeFormatterBuilder().appendPattern("d-M-uuuu[ H:m]")
            .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
            .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
            .toFormatter(),
        new DateTimeFormatterBuilder().appendPattern("uuuu-M-d[ H:m]")
            .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
            .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
            .toFormatter()
    );

    private static final List<String> VALID_DOCUMENT_DOMAINS = Arrays.asList("https://kgv.doffin.no",
            "http://eu.eu-supply.com", "http://permalink.mercell.com");
    private static final List<String> INVALID_TED_ID_PARTS = Arrays.asList("000-000000", "/S-");

    @Override
    protected final void registerSpecificPlugins() {
        Map<String, Map<Enum, List<String>>> lotMappings = new HashMap<>();
        lotMappings.put("countryMappings", countryMapping());
        lotMappings.put("selectionMethod", selectionMethodMapping());

        pluginRegistry
                .registerPlugin("integerPlugin", new IntegerPlugin(NUMBER_FORMAT))
                .registerPlugin("date", new DatePlugin<>(DATE_FORMATTERS))
                .registerPlugin("datetime", new DateTimePlugin<>(DATETIME_FORMATTERS))
                .registerPlugin("publications", new PublicationPlugin(NUMBER_FORMAT, DATETIME_FORMATTERS, formTypeMapping()))
                .registerPlugin("lots", new LotPlugin(NUMBER_FORMAT, DATETIME_FORMATTERS, lotMappings))
                .registerPlugin("prices", new PricePlugin(NUMBER_FORMAT))
                .registerPlugin("address", new AddressPlugin())
                .registerPlugin("bodies", new BodyPlugin(bodyTypeMapping(), mainActivitiesMapping(), countryMapping()))
                .registerPlugin("supplyType", new TenderSupplyTypePlugin(supplyTypeMapping()))
                .registerPlugin("selectionMethod", new SelectionMethodPlugin(selectionMethodMapping()))
                .registerPlugin("procedureType", new TenderProcedureTypePlugin(procedureTypeMapping()))
                .registerPlugin("awardCriteria", new AwardCriteriaPlugin(NUMBER_FORMAT));
    }

    /**
     * @return procedure type mapping.
     */
    private Map<Enum, List<String>> procedureTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderProcedureType.OPEN, Arrays.asList("pt_open"));
        mapping.put(TenderProcedureType.RESTRICTED, Arrays.asList("pt_restricted", "pt_accelerated_restricted",
            "pt_accelerated_restricted_choice"));
        mapping.put(TenderProcedureType.COMPETITIVE_DIALOG, Arrays.asList("pt_competitive_dialogue"));
        mapping.put(TenderProcedureType.INOVATION_PARTNERSHIP, Arrays.asList("pt_innovation_partnership"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITH_PUBLICATION,
            Arrays.asList("pt_accelerated_negotiated", "pt_negotiated_with_competition",
                "PT_AWARD_CONTRACT_WITH_PRIOR_PUBLICATION", "PT_COMPETITIVE_NEGOTIATION",
                "pt_negotiated_with_prior_call", "f02_pt_accelerated_negotiated"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITHOUT_PUBLICATION,
            Arrays.asList("PT_NEGOTIATED_WITHOUT_PUBLICATION", "f03_award_without_prior_publication",
                "f03_pt_negotiated_without_competition", "f06_pt_negotiated_without_competition",
                "f06_award_without_prior_publication", "PT_AWARD_CONTRACT_WITHOUT_CALL",
                "PT_AWARD_CONTRACT_WITHOUT_PUBLICATION"));
        mapping.put(TenderProcedureType.NEGOTIATED,
            Arrays.asList("NEGOTIATED", "pt_negotiated_choice", "PT_INVOLVING_NEGOTIATION"));

        return mapping;
    }



    /**
     * @return selection method mapping.
     */
    private Map<Enum, List<String>> selectionMethodMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(SelectionMethod.LOWEST_PRICE, Arrays.asList(SelectionMethod.LOWEST_PRICE.name()));
        mapping.put(SelectionMethod.MEAT, Arrays.asList(SelectionMethod.MEAT.name(),
            "MOST_ECONOMICALLY_ADVANTAGEOUS_TENDER", "MOST_ECONOMICALLY_ADVANTAGEOUS_TENDER_SHORT"));

        return mapping;
    }

    /**
     * @return main activities mapping.
     */
    private Map<Enum, List<String>> mainActivitiesMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(BuyerActivityType.GENERAL_PUBLIC_SERVICES, Arrays.asList("GENERAL_PUBLIC_SERVICES"));
        return mapping;
    }


    /**
     * @return form type mapping.
     */
    private Map<Enum, List<String>> formTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(PublicationFormType.CONTRACT_NOTICE, Arrays.asList("2", "82", "61", "51", "5", "102", "F102",
            "85", "F02", "F52", PublicationFormType.CONTRACT_NOTICE.name()));
        mapping.put(PublicationFormType.CONTRACT_AWARD, Arrays.asList("F03", "15", "3", "83", "6",
            PublicationFormType.CONTRACT_AWARD.name()));
        mapping.put(PublicationFormType.PRIOR_INFORMATION_NOTICE, Arrays.asList("F01", "1", "4", "7",
            PublicationFormType.PRIOR_INFORMATION_NOTICE.name()));

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
        // remove invalid TED publications
        parsedItem.setPublications(parsedItem.getPublications()
                .stream()
                .filter(p -> !INVALID_TED_ID_PARTS.stream().anyMatch(tedIdPart -> p.getSourceId().contains(tedIdPart)))
                .collect(Collectors.toList()));

        // remove invalid documents URLs
        if (parsedItem.getDocumentsLocation() != null) {
            final String documentsUrlLowerCase = parsedItem.getDocumentsLocation().getUrl().toLowerCase();
            if (!VALID_DOCUMENT_DOMAINS.stream().anyMatch(u -> documentsUrlLowerCase.startsWith(u))) {
                parsedItem.setDocumentsLocation(null);
            }
        }

        return parsedItem;
    }

    /**
     * @return body type mapping.
     */
    private Map<Enum, List<String>> bodyTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(BuyerType.NATIONAL_AGENCY, Arrays.asList("NATIONAL_AGENCY"));
        mapping.put(BuyerType.PUBLIC_BODY, Arrays.asList("BODY_PUBLIC"));
        mapping.put(BuyerType.REGIONAL_AUTHORITY, Arrays.asList("REGIONAL_AUTHORITY"));
        mapping.put(BuyerType.REGIONAL_AGENCY, Arrays.asList("REGIONAL_AGENCY"));
        mapping.put(BuyerType.NATIONAL_AUTHORITY, Arrays.asList("MINISTRY"));

        return mapping;
    }

    /**
     * @return country mapping
     */
    private Map<Enum, List<String>> countryMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(CountryCode.NO, Arrays.asList("NO"));

        return mapping;
    }

    /**
     * @return Supply type mapping
     */
    private Map<Enum, List<String>> supplyTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderSupplyType.SUPPLIES, Arrays.asList("SUPPLIES"));
        mapping.put(TenderSupplyType.SERVICES, Arrays.asList("SERVICES"));
        mapping.put(TenderSupplyType.WORKS, Arrays.asList("WORKS"));

        return mapping;
    }
}
