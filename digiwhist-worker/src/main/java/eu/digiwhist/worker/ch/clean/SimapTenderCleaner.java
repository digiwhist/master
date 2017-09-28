package eu.digiwhist.worker.ch.clean;

import eu.digiwhist.worker.clean.BaseDigiwhistTenderCleaner;
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
import eu.dl.worker.clean.plugin.DocumentPlugin;
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
 * Created by michalriha on 20/03/2017.
 */
public class SimapTenderCleaner extends BaseDigiwhistTenderCleaner {
    private static final String VERSION = "1";

    private static final NumberFormat NUMBER_FORMAT = NumberFormat.getInstance();

    private static final List<DateTimeFormatter> DATE_FORMATTERS = Arrays.asList(
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
                .registerPlugin("lots", new LotPlugin(NUMBER_FORMAT, DATE_FORMATTERS, new HashMap<>()));
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
        mapping.put(TenderSupplyType.SERVICES, Arrays.asList("Marché de services", "Order for services", "Commessa di" +
                " servizi", "Dienstleistungsauftrag"));
        mapping.put(TenderSupplyType.WORKS,
                Arrays.asList("Marché de travaux de construction", "Bauauftrag", "Commessa edile"));
        mapping.put(TenderSupplyType.SUPPLIES, Arrays.asList("Marché de fournitures", "Supply order", "Lieferauftrag"));

        return mapping;
    }
}
