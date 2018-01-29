package eu.digiwhist.worker.eu.clean;


import eu.digiwhist.worker.clean.BaseDigiwhistTenderCleaner;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.BuyerActivityType;
import eu.dl.dataaccess.dto.codetables.BuyerType;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.SelectionMethod;
import eu.dl.dataaccess.dto.codetables.TenderLotStatus;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.codetables.TenderSupplyType;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.AwardCriteriaPlugin;
import eu.dl.worker.clean.plugin.BodyPlugin;
import eu.dl.worker.clean.plugin.CorrigendumPlugin;
import eu.dl.worker.clean.plugin.DatePlugin;
import eu.dl.worker.clean.plugin.DateTimePlugin;
import eu.dl.worker.clean.plugin.DocumentPlugin;
import eu.dl.worker.clean.plugin.FundingsPlugin;
import eu.dl.worker.clean.plugin.IntegerPlugin;
import eu.dl.worker.clean.plugin.LotPlugin;
import eu.dl.worker.clean.plugin.NpwpReasonPlugin;
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
import java.time.format.SignStyle;
import java.time.temporal.ChronoField;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Cleaner for the TED.
 *
 * @author Tomas Mrazek
 */
public final class TedTenderCleaner extends BaseDigiwhistTenderCleaner {
    private static final String VERSION = "2";

    private static final List<NumberFormat> NUMBER_FORMATS = getNumberFormats();

    private static final List<DateTimeFormatter> DATE_FORMATTERS = new ArrayList<>(
            Arrays.asList(DateTimeFormatter.ofPattern("yyyy-MM-dd"), DateTimeFormatter.ofPattern("yyyy-M-d")));

    private static final DateTimeFormatter PUBLUCATIONS_PUBLICATIONDATE_FORMATTER =
            DateTimeFormatter.ofPattern("yyyyMMdd");

    /**
     * This DateTimeFormatter parses following datetime strings.
     * - yyyy-MM-dd (time is omitted)
     * - yyyy-MM-dd H:mm (hour as one digit)
     * - yyyy-MM-dd HH:mm (hour as two digits)
     */
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
            .registerPlugin("integerPlugin", new IntegerPlugin(NUMBER_FORMATS))
            .registerPlugin("supplyType", new TenderSupplyTypePlugin(supplyTypeMapping()))
            .registerPlugin("procedureType",
                new TenderProcedureTypePlugin(procedureTypeMapping(), acceleratedProcedures()))
            .registerPlugin("date", new DatePlugin(DATE_FORMATTERS))
            .registerPlugin("datetime", new DateTimePlugin(DATETIME_FORMATTER))
            .registerPlugin("bodies", new BodyPlugin(bodyTypeMapping(), bodyActivityMapping()))
            .registerPlugin("publications",
                new PublicationPlugin(NUMBER_FORMATS, DATE_FORMATTERS, formTypeMapping())
                    .addFormatter(PUBLUCATIONS_PUBLICATIONDATE_FORMATTER))
            //document type isn't provided -> mapping = null
            //selection method for lots isn't provided -> mapping = null
            //price unit isn't provided -> mapping = null
            .registerPlugin("lots", new LotPlugin(NUMBER_FORMATS, DATE_FORMATTERS,
                Collections.singletonMap("statusMapping", lotStatusMapping())))
            //document type isn't provided -> mapping = null
            .registerPlugin("documents", new DocumentPlugin(NUMBER_FORMATS, DATE_FORMATTERS, null))
            .registerPlugin("prices", new PricePlugin(NUMBER_FORMATS))
            .registerPlugin("fundings", new FundingsPlugin(NUMBER_FORMATS))
            .registerPlugin("corrections", new CorrigendumPlugin(NUMBER_FORMATS, DATE_FORMATTERS))
            .registerPlugin("awardCriteria", new AwardCriteriaPlugin(NUMBER_FORMATS))
            .registerPlugin("selectionMethod", new SelectionMethodPlugin(getSelectionMethodMapping()))
            .registerPlugin("npwpReasons", new NpwpReasonPlugin(npwpReasonMapping()));
    }

    @Override
    public String getVersion() {
        return VERSION;
    }

    /**
     * @return lot status mapping
     */
    private static Map<Enum, List<String>> lotStatusMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        
        mapping.put(TenderLotStatus.AWARDED, Arrays.asList("awarded_contract"));
        mapping.put(TenderLotStatus.CANCELLED, Arrays.asList("no_awarded_contract", "procurement_discontinued",
            "procurement_unsuccessful"));

        return mapping;
    }

    @Override
    protected CleanTender postProcessSourceSpecificRules(final ParsedTender parsedTender,
                                                         final CleanTender cleanTender) {

        /* publication.sourceId fix, leading zero removing + normalization for specific subgroup of strings
            - 2011/S 017–281404 > 2011/S 17-281404
            - 2011/S17-281404   > 2011/S 17-281404
            - 2011/S 0-281404   > 2011/S 281404
            - 2011/S0–281404    > 2011/S 281404
            - 2011/S -281404    > 2011/S 281404
            - 2011/S0281404     > 2011/S 281404
         */
        if (cleanTender.getPublications() != null) {
            cleanTender.getPublications().stream()
                .filter(Objects::nonNull)
                .forEach(p -> {
                    String sourceId = p.getSourceId();                    
                    if (sourceId != null && sourceId.matches("\\d{4}/S ?(\\d*(\\-|–))?\\d+")) {
                        p.setSourceId(sourceId
                            .replaceAll("(?<=S ?)0*(\\-|–)?", "")   // remove leading zero
                            .replaceAll("S(?! )", "S ")             // insert space after 'S' if needed
                            .replace("–", "-"));                    // normalize dash
                    }
                });
        }

        return cleanTender;
    }

    /**
     * @return npwp reason type mapping for cleaning process
     */
    private static Map<Enum, List<String>> npwpReasonMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        /*
        R2.08
        ADDITIONAL_WORKS
        AIR_MARITIME_TRANSPORT_FOR_ARMED_FORCES_DEPLOYMENT
        CONTRACT_RESEARCH_DIRECTIVE
        CONTRACT_SERVICES_LISTED_IN_DIRECTIVE
        CONTRACT_SERVICES_OUTSIDE_DIRECTIVE
        CONTRACTS_AWARDED_FRAMEWORK_DIRECTIVE
        EXTREME_URGENCY_EVENTS_UNFORESEEABLE
        MANUFACTURED_BY_DIRECTIVE
        ONLY_IRREGULAR_INACCEPTABLE_TENDERERS
        OTHER_JUSTIFICATION
        PERIOD_FOR_PROCEDURE_INCOMPATIBLE_WITH_CRISIS
        PURCHASE_SUPPLIES_ADVANTAGEOUS_TERMS
        REASON_CONTRACT_LAWFUL
        REASONS_PROVIDED_PARTICULAR_TENDERER
        REASONS_PROVIDED_PARTICULAR_TENDERER_EXCLUSIVE_RIGHTS
        REASONS_PROVIDED_PARTICULAR_TENDERER_TECHNICAL
        SERVICE_CONTRACT_AWARDED_SUCCESSFUL_CANDIDATE
        SUPPLIES_QUOTED_PURCHASED_COMMODITY_MARKET
        WORKS_REPETITION_EXISTING_WORKS

        //negations
        NO_ADDITIONAL_WORKS
        NO_AIR_MARITIME_TRANSPORT_FOR_ARMED_FORCES_DEPLOYMENT
        NO_CONTRACT_RESEARCH_DIRECTIVE
        NO_CONTRACTS_AWARDED_FRAMEWORK_DIRECTIVE
        NO_EXTREME_URGENCY_EVENTS_UNFORESEEABLE
        NO_MANUFACTURED_BY_DIRECTIVE
        NO_ONLY_IRREGULAR_INACCEPTABLE_TENDERERS
        NO_PERIOD_FOR_PROCEDURE_INCOMPATIBLE_WITH_CRISIS
        NO_SERVICE_CONTRACT_AWARDED_SUCCESSFUL_CANDIDATE
        NO_SUPPLIES_QUOTED_PURCHASED_COMMODITY_MARKET
        NO_WORKS_REPETITION_EXISTING_WORKS



        R2.09
        D_ADD_DELIVERIES_ORDERED
        D_ALL_TENDERS
        D_ARTISTIC
        D_BARGAIN_PURCHASE
        D_COMMODITY_MARKET
        D_CONTRACT_AWARDED_DESIGN_CONTEST
        D_EXCLUSIVE_RIGHT
        D_EXTREME_URGENCY
        D_FROM_LIQUIDATOR_CREDITOR
        D_FROM_WINDING_PROVIDER
        D_MARITIME_SERVICES
        D_NO_TENDERS_REQUESTS
        D_OTHER_SERVICES
        D_OUTSIDE_SCOPE
        D_PERIODS_INCOMPATIBLE
        D_PROC_COMPETITIVE_DIALOGUE
        D_PROC_NEGOTIATED_PRIOR_CALL_COMPETITION
        D_PROC_OPEN
        D_PROC_RESTRICTED
        D_PROTECT_RIGHTS
        D_PURE_RESEARCH
        D_REPETITION_EXISTING
        D_SERVICES_LISTED
        D_TECHNICAL
        */

        // mapping.put(NpwpReason., Arrays.asList());

        return mapping;
    }

    /**
     * @return supply type mapping for cleaning process
     */
    private static Map<Enum, List<String>> supplyTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(TenderSupplyType.SERVICES, Arrays.asList("SERVICES"));
        mapping.put(TenderSupplyType.WORKS, Arrays.asList("WORKS"));
        mapping.put(TenderSupplyType.SUPPLIES, Arrays.asList("SUPPLIES"));

        return mapping;
    }

    /**
     * @return procedure type mapping for cleaning process
     */
    private static Map<Enum, List<String>> procedureTypeMapping() {
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
     * @return list of accelerated procedures
     */
    private static List<String> acceleratedProcedures() {
        return Arrays.asList("pt_accelerated_negotiated", "pt_accelerated_restricted_choice",
            "pt_accelerated_restricted", "f02_pt_accelerated_negotiated");
    }

    /**
     * @return TED body activities mapping
     */
    private static Map<Enum, List<String>> bodyActivityMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(BuyerActivityType.DEFENCE, Arrays.asList("DEFENCE"));
        mapping.put(BuyerActivityType.ECONOMIC_AND_FINANCIAL_AFFAIRS, Arrays.asList("ECONOMIC_AND_FINANCIAL_AFFAIRS"));
        mapping.put(BuyerActivityType.EDUCATION, Arrays.asList("EDUCATION"));
        mapping.put(BuyerActivityType.ENVIRONMENT, Arrays.asList("ENVIRONMENT"));
        mapping.put(BuyerActivityType.GENERAL_PUBLIC_SERVICES, Arrays.asList("GENERAL_PUBLIC_SERVICES"));
        mapping.put(BuyerActivityType.HEALTH, Arrays.asList("HEALTH"));
        mapping.put(
            BuyerActivityType.HOUSING_AND_COMMUNITY_AMENITIES, Arrays.asList("HOUSING_AND_COMMUNITY_AMENITIES"));
        mapping.put(BuyerActivityType.PUBLIC_ORDER_AND_SAFETY, Arrays.asList("PUBLIC_ORDER_AND_SAFETY"));
        mapping.put(
            BuyerActivityType.RECREATION_CULTURE_AND_RELIGION, Arrays.asList("RECREATION_CULTURE_AND_RELIGION",
                "Recreation, culture and religion"));
        mapping.put(BuyerActivityType.SOCIAL_PROTECTION, Arrays.asList("SOCIAL_PROTECTION"));
        mapping.put(BuyerActivityType.AIRPORT, Arrays.asList("AIRPORT_RELATED_ACTIVITIES",
            "Airport-related activities"));
        mapping.put(BuyerActivityType.ELECTRICITY, Arrays.asList("ELECTRICITY"));
        mapping.put(
            BuyerActivityType.COAL_AND_OTHER_EXTRACTION, Arrays.asList("EXPLORATION_EXTRACTION_COAL_OTHER_SOLID_FUEL",
                "Exploration and extraction of coal and other solid fuels"));
        mapping.put(BuyerActivityType.GAS_AND_OIL_EXTRACTION, Arrays.asList("EXPLORATION_EXTRACTION_GAS_OIL",
            "Exploration and extraction of gas and oil"));
        mapping.put(BuyerActivityType.PORT, Arrays.asList("PORT_RELATED_ACTIVITIES", "Port-related activities"));
        mapping.put(BuyerActivityType.POSTAL, Arrays.asList("POSTAL_SERVICES"));
        mapping.put(
            BuyerActivityType.GAS_AND_HEAT_PRODUCTION, Arrays.asList("PRODUCTION_TRANSPORT_DISTRIBUTION_GAS_HEAT",
                "Production, transport and distribution of gas and heat"));
        mapping.put(BuyerActivityType.RAILWAY, Arrays.asList("RAILWAY_SERVICES"));
        mapping.put(BuyerActivityType.URBAN_TRANSPORT, Arrays.asList("URBAN_RAILWAY_TRAMWAY_TROLLEYBUS_BUS_SERVICES",
            "Urban railway, tramway, trolleybus or bus services"));
        mapping.put(BuyerActivityType.WATER, Arrays.asList("WATER"));
        mapping.put(BuyerActivityType.OTHER, Arrays.asList("OTHER"));

        // extend each mapping with values without underscore if needed
        mapping.entrySet().forEach(m -> {
            List<String> extendedMapping = new ArrayList<>();
            for (String n : m.getValue()) {
                if (n.contains("_")) {
                    extendedMapping.add(n.replace("_", ""));
                }
            }
            if (!extendedMapping.isEmpty()) {
                extendedMapping.addAll(m.getValue());
                m.setValue(extendedMapping);
            }
        });

        return mapping;
    }

    /**
     * @return TED body type mapping
     */
    private static Map<Enum, List<String>> bodyTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(BuyerType.PUBLIC_BODY, Arrays.asList("BODY_PUBLIC", "Body governed by public law"));
        mapping.put(BuyerType.NATIONAL_AGENCY, Arrays.asList("NATIONAL_AGENCY", "National or federal Agency/Office"));
        mapping.put(BuyerType.NATIONAL_AUTHORITY, Arrays.asList("MINISTRY", "Ministry or any other national or federal"
            + " authority"));
        mapping.put(BuyerType.REGIONAL_AGENCY, Arrays.asList("REGIONAL_AGENCY", "Regional or local Agency/Office"));
        mapping.put(BuyerType.REGIONAL_AUTHORITY, Arrays.asList("REGIONAL_AUTHORITY", "Regional or local authority"));
        mapping.put(BuyerType.EUROPEAN_AGENCY, Arrays.asList("EU_INSTITUTION", "European Institution/Agency or"
            + " International Organisation"));
        mapping.put(BuyerType.UTILITIES, Arrays.asList("Utilities", "Utilities entity"));
        mapping.put(BuyerType.OTHER, Arrays.asList("Other"));

        return mapping;
    }

    /**
     * @return form type mapping
     */
    private Map<Enum, List<String>> formTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(PublicationFormType.CONTRACT_NOTICE, Arrays.asList("F02_2011", "F02_2014", "F05_2011", "F05_2014",
            "Contract notice", "Dynamic purchasing system"));
        mapping.put(PublicationFormType.CONTRACT_AWARD, Arrays.asList("F03_2011", "F03_2014", "F06_2011", "F06_2014",
            "Contract award notice", "Contract award", "VOLUNTARY_EX_ANTE_TRANSPARENCY_NOTICE"));
        mapping.put(PublicationFormType.OTHER, Arrays.asList("other_previous_publication", "NOTICE_BUYER_PROFILE"));
        mapping.put(PublicationFormType.PRIOR_INFORMATION_NOTICE, Arrays.asList("F01_2011", "F01_2014",
            "PRIOR_INFORMATION_NOTICE"));
        mapping.put(PublicationFormType.CONTRACT_UPDATE, Arrays.asList("F14_2011", "F14_2014",
            "Additional information"));

        return mapping;
    }

    /**
     * @return selection method plugin
     */
    private Map<Enum, List<String>> getSelectionMethodMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(SelectionMethod.MEAT, Arrays.asList("The most economic tender"));
        mapping.put(SelectionMethod.LOWEST_PRICE, Arrays.asList("Lowest price"));
        mapping.put(null, Arrays.asList("Not specified"));

        return mapping;
    }

    /**
     * @return decimal number formats
     */
    private static List<NumberFormat> getNumberFormats() {
        DecimalFormat formatComma = (DecimalFormat) NumberFormat.getNumberInstance();
        DecimalFormatSymbols symbolsComa = formatComma.getDecimalFormatSymbols();
        symbolsComa.setDecimalSeparator(',');
        symbolsComa.setGroupingSeparator(' ');
        formatComma.setDecimalFormatSymbols(symbolsComa);
        
        DecimalFormat formatDot = (DecimalFormat) NumberFormat.getNumberInstance();
        DecimalFormatSymbols symbolsDot = formatDot.getDecimalFormatSymbols();
        symbolsDot.setDecimalSeparator('.');
        symbolsDot.setGroupingSeparator(' ');
        formatDot.setDecimalFormatSymbols(symbolsDot);

        return Arrays.asList(formatComma, formatDot);
    }

    @Override
    protected ParsedTender preProcessParsedItem(final ParsedTender parsedItem) {
        return parsedItem;
    }
}
