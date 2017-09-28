package eu.digiwhist.worker.ie.clean;

import eu.digiwhist.worker.clean.BaseDigiwhistTenderCleaner;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.CountryCode;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.TenderSupplyType;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.AddressPlugin;
import eu.dl.worker.clean.plugin.AwardCriteriaPlugin;
import eu.dl.worker.clean.plugin.BodyPlugin;
import eu.dl.worker.clean.plugin.DateTimePlugin;
import eu.dl.worker.clean.plugin.LotPlugin;
import eu.dl.worker.clean.plugin.PublicationPlugin;
import eu.dl.worker.clean.plugin.TenderSupplyTypePlugin;

import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Created by michalriha on 11/05/2017.
 */
public class ETendersTenderCleaner extends BaseDigiwhistTenderCleaner {
    private static final String VERSION = "1";

    private static final NumberFormat NUMBER_FORMAT = NumberFormat.getInstance();

    private static final List<DateTimeFormatter> DATE_FORMATTERS = Arrays.asList(
            DateTimeFormatter.ofPattern("dd-MM-uuuu"),
            DateTimeFormatter.ofPattern("dd-MM-uuuu HH:mm"));

    @SuppressWarnings("unchecked")
    @Override
    protected final void registerSpecificPlugins() {
        pluginRegistry
                .registerPlugin("publication", new PublicationPlugin(NUMBER_FORMAT, DATE_FORMATTERS,
                        formTypeMapping()))
                .registerPlugin("address", new AddressPlugin())
                .registerPlugin("body", new BodyPlugin(null, null, countryMapping()))
                .registerPlugin("awardCriteria", new AwardCriteriaPlugin(NUMBER_FORMAT))
                .registerPlugin("supplyType", new TenderSupplyTypePlugin(supplyTypeMapping()))
                .registerPlugin("lots", new LotPlugin(NUMBER_FORMAT, DATE_FORMATTERS, null))
                .registerPlugin("dateTime", new DateTimePlugin(DATE_FORMATTERS));
    }

    /**
     * @return Supply type mapping.
     */
    private Map<Enum, List<String>> supplyTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderSupplyType.SERVICES, Arrays.asList("Services"));
        mapping.put(TenderSupplyType.SUPPLIES, Arrays.asList("Supplies"));
        mapping.put(TenderSupplyType.WORKS, Arrays.asList("Works"));

        return mapping;
    }

    @Override
    public final String getVersion() {
        return VERSION;
    }

    @Override
    protected final CleanTender postProcessSourceSpecificRules(final ParsedTender parsedItem, final CleanTender
            cleanItem) {
        return cleanItem;
    }

    @Override
    protected final ParsedTender preProcessParsedItem(final ParsedTender parsedItem) {
        if (parsedItem.getCpvs() != null) {
            parsedItem.getCpvs().forEach(
                    t -> t.setCode(t.getCode() == null ? null : t.getCode().replaceAll(" .*", "")));
        }

        if (parsedItem.getBidDeadline() != null) {
            parsedItem.setBidDeadline(parsedItem.getBidDeadline().replaceAll("\\D* ", ""));
        }

        if (parsedItem.getBuyers().get(0).getContactName() != null) {
            parsedItem.getBuyers().get(0).setContactName(parsedItem.getBuyers().get(0).getContactName()
                    .replaceAll("\\[.*\\]", ""));
        }

        return parsedItem;
    }

    /**
     * @return form type mapping
     */
    private Map<Enum, List<String>> countryMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(CountryCode.AU, Arrays.asList("Australia"));
        mapping.put(CountryCode.IS, Arrays.asList("Iceland"));
        mapping.put(CountryCode.IE, Arrays.asList("Ireland"));
        mapping.put(CountryCode.IL, Arrays.asList("Israel"));
        mapping.put(CountryCode.SE, Arrays.asList("Sweden"));
        mapping.put(CountryCode.GB, Arrays.asList("United Kingdom"));
        mapping.put(CountryCode.US, Arrays.asList("United States of America"));

        return mapping;
    }

    /**
     * @return Form type mapping.
     */
    private Map<Enum, List<String>> formTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(PublicationFormType.CONTRACT_NOTICE, Arrays.asList(
                "Concession notice (TED (v209))",
                "Contract Notice (eTenders)",
                "Contract Notice (TED (2.0.7 old not supported version))",
                "Contract Notice (TED (v208))",
                "Contract notice (TED (v209))",
                "Contract notice - concession (TED (2.0.7 old not supported version))",
                "Contract notice - concession (TED (v208))",
                "Contract Notice - Utilities (TED (2.0.7 old not supported version))",
                "Contract Notice - Utilities (TED (v208))",
                "Contract notice - utilities (TED (v209))",
                "Contract notice for contracts in the field of defence and security (TED (v208))",
                "Notice for Additional information (TED (2.0.7 old not supported version))",
                "Notice for Additional information (TED (v208))",
                "Notice on a buyer profile (TED (2.0.7 old not supported version))",
                "Design contest notice (TED (2.0.7 old not supported version))",
                "Design contest notice (TED (v208))",
                "Design contest notice (TED (v209))",
                "Dynamic purchasing system (TED (2.0.7 old not supported version))",
                "Periodic indicative Notice - Utilities (TED (2.0.7 old not supported version))",
                "Periodic indicative Notice - Utilities (TED (v208))",
                "Periodic indicative Notice - Utilities (TED (v209))",
                "Voluntary Ex Ante Transparency Notice (TED (2.0.7 old not supported version))"));
        mapping.put(PublicationFormType.CONTRACT_AWARD, Arrays.asList(
                "Concession award notice (TED (v209))",
                "Contract Award Notice (eTenders)",
                "Contract Award Notice (TED (2.0.7 old not supported version))",
                "Contract Award Notice (TED (v208))",
                "Contract award notice (TED (v209))",
                "Contract Award Notice - Utilities (TED (2.0.7 old not supported version))",
                "Contract Award Notice - Utilities (TED (v208))",
                "Contract award notice - utilities (TED (v209))"));
        mapping.put(PublicationFormType.PRIOR_INFORMATION_NOTICE, Arrays.asList(
                "Prior Information Notice (eTenders)",
                "Prior Information Notice (TED (2.0.7 old not supported version))",
                "Prior Information Notice (TED (v208))",
                "Prior information notice (TED (v209))",
                "Prior information notice - Call for competition (TED (v209))"));
        mapping.put(PublicationFormType.OTHER, Arrays.asList(
                "Corrigendum (TED (v209))",
                "Public Works Concession (TED (2.0.7 old not supported version))",
                "Qualification system - utilities (TED (2.0.7 old not supported version))",
                "Qualification system - utilities - Call for competition (TED (v209))",
                "Results of design contest (TED (2.0.7 old not supported version))",
                "Social and other specific services – public contracts (TED (v209))",
                "Social and other specific services – public contracts - Call for competition (TED (v209))",
                "Social and other specific services – utilities (TED (v209))"));

        return mapping;
    }
}
