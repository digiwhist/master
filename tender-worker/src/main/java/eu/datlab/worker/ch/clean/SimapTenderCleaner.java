package eu.datlab.worker.ch.clean;

import eu.datlab.worker.clean.BaseDatlabTenderCleaner;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.BuyerType;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.TenderSupplyType;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.AddressPlugin;
import eu.dl.worker.clean.plugin.AwardCriteriaPlugin;
import eu.dl.worker.clean.plugin.BodyPlugin;
import eu.dl.worker.clean.plugin.CpvPlugin;
import eu.dl.worker.clean.plugin.DatePlugin;
import eu.dl.worker.clean.plugin.DateTimePlugin;
import eu.dl.worker.clean.plugin.DocumentPlugin;
import eu.dl.worker.clean.plugin.LotPlugin;
import eu.dl.worker.clean.plugin.PublicationPlugin;
import eu.dl.worker.clean.plugin.TenderSupplyTypePlugin;
import org.apache.commons.lang3.StringUtils;

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
 * Created by michalriha on 20/03/2017.
 */
public class SimapTenderCleaner extends BaseDatlabTenderCleaner {
    private static final String VERSION = "1";

    private static final NumberFormat NUMBER_FORMAT;

    private static final Locale LOCALE = new Locale("ch");
    static {
        DecimalFormatSymbols formatSymbols = new DecimalFormatSymbols(LOCALE);
        formatSymbols.setDecimalSeparator('.');
        formatSymbols.setGroupingSeparator('\'');
        NUMBER_FORMAT = new DecimalFormat("#,##0.###", formatSymbols);
    }

    private static final List<DateTimeFormatter> DATE_FORMATTERS = Arrays.asList(
            new DateTimeFormatterBuilder()
                    .appendPattern("dd.MM.uuuu[ HH:mm[:s]]")
                    .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
                    .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
                    .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
                    .toFormatter(LOCALE),
            new DateTimeFormatterBuilder()
                    .appendPattern("d. M. uuuu[ HH:mm[:s]]")
                    .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
                    .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
                    .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
                    .toFormatter(LOCALE),
            DateTimeFormatter.ofPattern("d. M. uuuu"),
            DateTimeFormatter.ofPattern("d. M. uuuu."),
            DateTimeFormatter.ofPattern("dd.MM.uuuu"),
            DateTimeFormatter.ofPattern("dd.MM.uuuu."),
            DateTimeFormatter.ofPattern("dd.MM.uuuu "));


    @SuppressWarnings("unchecked")
    @Override
    protected final void registerSpecificPlugins() {
        pluginRegistry
                .registerPlugin("publication", new PublicationPlugin(NUMBER_FORMAT, DATE_FORMATTERS,
                        formTypeMapping()))
                .registerPlugin("address", new AddressPlugin())
                .registerPlugin("body", new BodyPlugin(buyerTypeMapping(), null, null))
                .registerPlugin("cpv", new CpvPlugin())
                .registerPlugin("awardCriteria", new AwardCriteriaPlugin(NUMBER_FORMAT))
                .registerPlugin("documents", new DocumentPlugin(NUMBER_FORMAT, DATE_FORMATTERS, null))
                .registerPlugin("supplyType", new TenderSupplyTypePlugin(supplyTypeMapping()))
                .registerPlugin("lots", new LotPlugin(NUMBER_FORMAT, DATE_FORMATTERS, new HashMap<>()))
                .registerPlugin("date", new DatePlugin(DATE_FORMATTERS))
                .registerPlugin("datetime", new DateTimePlugin(DATE_FORMATTERS));
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
        if (parsedItem.getBuyerAssignedId() != null) {
            parsedItem.setBuyerAssignedId(parsedItem.getBuyerAssignedId().replace("Project ID", ""));
        }

        if (parsedItem.getPublications() != null) {
            for (ParsedPublication publication : parsedItem.getPublications()) {
                if (publication.getBuyerAssignedId() != null) {
                    publication.setBuyerAssignedId(publication.getBuyerAssignedId().replace("Project ID", ""));
                }

                if (publication.getSourceId() != null) {
                    publication.setSourceId(publication.getSourceId().replace("Notice no.", ""));
                }
            }
        }

        if (parsedItem.getIsAcceleratedProcedure() != null) {
            parsedItem.setIsAcceleratedProcedure(parseToBoolean(parsedItem.getIsAcceleratedProcedure()));
        }

        if (parsedItem.getIsAwarded() != null) {
            parsedItem.setIsAwarded(parseToBoolean(parsedItem.getIsAwarded()));
        }

        if (parsedItem.getDocumentsPayable() != null) {
            parsedItem.setDocumentsPayable(parseToBoolean(parsedItem.getDocumentsPayable()));
        }

        if (parsedItem.getIsDocumentsAccessRestricted() != null) {
            parsedItem.setIsDocumentsAccessRestricted(parseToBoolean(parsedItem.getIsDocumentsAccessRestricted()));
        }

        if (parsedItem.getIsCentralProcurement() != null) {
            parsedItem.setIsCentralProcurement(parseToBoolean(parsedItem.getIsCentralProcurement()));
        }

        if (parsedItem.getIsJointProcurement() != null) {
            parsedItem.setIsJointProcurement(parseToBoolean(parsedItem.getIsJointProcurement()));
        }

        if (parsedItem.getIsOnBehalfOf() != null) {
            parsedItem.setIsOnBehalfOf(parseToBoolean(parsedItem.getIsOnBehalfOf()));
        }

        if (parsedItem.getHasLots() != null) {
            parsedItem.setHasLots(parseToBoolean(parsedItem.getHasLots()));
        }

        if (parsedItem.getAreVariantsAccepted() != null) {
            parsedItem.setAreVariantsAccepted(parseToBoolean(parsedItem.getAreVariantsAccepted()));
        }

        if (parsedItem.getHasOptions() != null) {
            parsedItem.setHasOptions(parseToBoolean(parsedItem.getHasOptions()));
        }

        if (parsedItem.getIsCoveredByGpa() != null) {
            parsedItem.setIsCoveredByGpa(parseToBoolean(parsedItem.getIsCoveredByGpa()));
        }

        if (parsedItem.getIsFrameworkAgreement() != null) {
            parsedItem.setIsFrameworkAgreement(parseToBoolean(parsedItem.getIsFrameworkAgreement()));
        }

        if (parsedItem.getIsDps() != null) {
            parsedItem.setIsDps(parseToBoolean(parsedItem.getIsDps()));
        }

        if (parsedItem.getIsElectronicAuction() != null) {
            parsedItem.setIsElectronicAuction(parseToBoolean(parsedItem.getIsElectronicAuction()));
        }

        if (parsedItem.getIsFrameworkAgreement() != null) {
            parsedItem.setIsFrameworkAgreement(parseToBoolean(parsedItem.getIsFrameworkAgreement()));
        }

        if (parsedItem.getIsWholeTenderCancelled() != null) {
            parsedItem.setIsWholeTenderCancelled(parseToBoolean(parsedItem.getIsWholeTenderCancelled()));
        }

        if (parsedItem.getIsEInvoiceAccepted() != null) {
            parsedItem.setIsEInvoiceAccepted(parseToBoolean(parsedItem.getIsEInvoiceAccepted()));
        }

        return parsedItem;
    }

    /**
     * @return Form type mapping.
     */
    private Map<Enum, List<String>> formTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(PublicationFormType.CONTRACT_NOTICE, Arrays.asList("Invitations to tender (summary)",
                "Invitation to tender"));
        mapping.put(PublicationFormType.CONTRACT_AWARD, Arrays.asList("Contracts awarded", "Competitions awarded"));
        mapping.put(PublicationFormType.CONTRACT_CANCELLATION, Arrays.asList("Cancellation of invitation to tender",
                "Revocation"));
        mapping.put(PublicationFormType.CONTRACT_UPDATE, Arrays.asList("Competitions      Correction", "Cancellation " +
                        "of invitation to tender       Correction", "Rectification      Correction", "Prior " +
                        "information " +
                        "notice      Correction", "Selection of participants       Correction",
                "Revocation      Correction",
                "Invitation to tender       Correction", "Invitations to tender (summary)      Correction",
                "Contracts awarded      Correction", "Competitions awarded      Correction"));
        mapping.put(PublicationFormType.PRIOR_INFORMATION_NOTICE, Arrays.asList("Prior information notice"));
        mapping.put(PublicationFormType.OTHER, Arrays.asList("Selection of participants", "Competitions"));

        return mapping;
    }

    /**
     * @return Buyer type mapping.
     */
    private Map<Enum, List<String>> buyerTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(BuyerType.NATIONAL_AGENCY, Arrays.asList("Bund (Dezentrale Bundesverwaltung - öffentlich rec" +
                "htliche Organisationen)", "Confédération (Administration fédérale décentralisée - organisations de d" +
                "roit public de la Confédération)", "Confederazione (Amministrazione federale decentralizzata - azie" +
                "nde federali di diritto pubblico)", "Confederation (Decentralized federal administration -public-l" +
                "egal organizations of the federation)"));
        mapping.put(BuyerType.NATIONAL_AUTHORITY, Arrays.asList("Bund (Zentrale Bundesverwaltung)", "Confédérati" +
                "on (Administration fédérale centrale)", "Confederazione (Amministrazione federale centrale)", "Conf" +
                "ederation (Central federal administration)"));
        mapping.put(BuyerType.REGIONAL_AGENCY, Arrays.asList("Andere Träger kommunaler Aufgaben", "Andere Träge" +
                "r kantonaler Aufgaben", "Autres collectivités assumant des tâches cantonales", "Autres collectivit" +
                "és assumant des tâches communales", "Cantone", "Other supporting organisations assuming cantonal t" +
                "asks", "Altri enti preposti a compiti comunali"));
        mapping.put(BuyerType.REGIONAL_AUTHORITY, Arrays.asList("Kanton", "Gemeinde/Stadt", "Commune/Ville",
                "Canton", "Comune/Città"));
        mapping.put(BuyerType.OTHER, Arrays.asList("Andere", "Autres", "Other"));

        return mapping;
    }

    /**
     * @return supply type mapping for cleaning process
     */
    private static Map<Enum, List<String>> supplyTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(TenderSupplyType.SERVICES, Arrays.asList("Marché de services", "Order for services", "Commessa di servizi",
            "Dienstleistungsauftrag"));
        mapping.put(TenderSupplyType.WORKS, Arrays.asList("Marché de travaux de construction", "Bauauftrag", "Commessa edile",
            "Construction order"));
        mapping.put(TenderSupplyType.SUPPLIES, Arrays.asList("Marché de fournitures", "Supply order", "Lieferauftrag",
            "Commessa di forniture"));

        mapping.put(null, Arrays.asList("Ja", "Nein", "Non", "Yes", "No", "Oui"));

        return mapping;
    }

    /**
     * Check weather string contains text parsable to boolean.
     *
     * @param string string to clean
     * @return String or null
     */
    private static String parseToBoolean(final String string) {
        if (string == null) {
            return null;
        } else if (StringUtils.containsIgnoreCase("Ja", string)
                || StringUtils.containsIgnoreCase("True", string)
                || StringUtils.containsIgnoreCase("Yes", string)
                || StringUtils.containsIgnoreCase("Oui", string)) {
            return Boolean.TRUE.toString();
        } else if (StringUtils.containsIgnoreCase("Nein", string)
                || StringUtils.containsIgnoreCase("False", string)
                || StringUtils.containsIgnoreCase("No", string)
                || StringUtils.containsIgnoreCase("Non", string)) {
            return Boolean.FALSE.toString();
        } else {
            return string;
        }
    }
}
