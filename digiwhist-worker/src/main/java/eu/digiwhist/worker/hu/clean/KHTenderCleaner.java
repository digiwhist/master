package eu.digiwhist.worker.hu.clean;

import eu.digiwhist.worker.clean.BaseDigiwhistTenderCleaner;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.CountryCode;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.AddressPlugin;
import eu.dl.worker.clean.plugin.BodyPlugin;
import eu.dl.worker.clean.plugin.DatePlugin;
import eu.dl.worker.clean.plugin.DateTimePlugin;
import eu.dl.worker.clean.plugin.IntegerPlugin;
import eu.dl.worker.clean.plugin.LotPlugin;
import eu.dl.worker.clean.plugin.PricePlugin;
import eu.dl.worker.clean.plugin.PublicationPlugin;
import eu.dl.worker.clean.plugin.TenderSupplyTypePlugin;

import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Created by michalriha on 26/04/2017.
 */
public class KHTenderCleaner extends BaseDigiwhistTenderCleaner {
    private static final String VERSION = "1.0";

    private static final NumberFormat NUMBER_FORMAT = NumberFormat.getInstance();

    private static final List<DateTimeFormatter> DATE_FORMATTERS = Arrays.asList(
            DateTimeFormatter.ofPattern("uuuu.MM.dd"),
            DateTimeFormatter.ofPattern("uuuu.MM.dd."));

    private static final List<DateTimeFormatter> DATETIME_FORMATTERS = Arrays.asList(
            DateTimeFormatter.ISO_LOCAL_DATE_TIME, DateTimeFormatter.ISO_OFFSET_DATE_TIME);

    @SuppressWarnings("unchecked")
    @Override
    protected final void registerSpecificPlugins() {
        Map<String, Map<Enum, List<String>>> lotMappings = new HashMap<>();
        lotMappings.put("countryMappings", countryMapping());

        pluginRegistry
                .registerPlugin("integerPlugin", new IntegerPlugin(NUMBER_FORMAT))
                .registerPlugin("date", new DatePlugin<>(DATE_FORMATTERS))
                .registerPlugin("datetime", new DateTimePlugin<>(DATETIME_FORMATTERS))
                .registerPlugin("publications",
                        new PublicationPlugin(NUMBER_FORMAT, DATETIME_FORMATTERS, formTypeMapping()))
                .registerPlugin("lots", new LotPlugin(NUMBER_FORMAT, DATETIME_FORMATTERS, lotMappings))
                .registerPlugin("prices", new PricePlugin(NUMBER_FORMAT))
                .registerPlugin("address", new AddressPlugin())
                .registerPlugin("bodies", new BodyPlugin(null, null, countryMapping()))
                .registerPlugin("supplyType", new TenderSupplyTypePlugin(null));
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

    /**
     * @return country mapping
     */
    private Map<Enum, List<String>> countryMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(CountryCode.HU, Arrays.asList("HU", "Magyarorszag", "Magyarország"));
        mapping.put(CountryCode.AT, Arrays.asList("AT"));
        mapping.put(CountryCode.BE, Arrays.asList("BE"));
        mapping.put(CountryCode.BG, Arrays.asList("BG"));
        mapping.put(CountryCode.CH, Arrays.asList("CH"));
        mapping.put(CountryCode.CN, Arrays.asList("CN"));
        mapping.put(CountryCode.CZ, Arrays.asList("CZ"));
        mapping.put(CountryCode.DE, Arrays.asList("DE"));
        mapping.put(CountryCode.ES, Arrays.asList("ES"));
        mapping.put(CountryCode.FI, Arrays.asList("FI"));
        mapping.put(CountryCode.FR, Arrays.asList("FR"));
        mapping.put(CountryCode.GQ, Arrays.asList("GQ"));
        mapping.put(CountryCode.HR, Arrays.asList("HR"));
        mapping.put(CountryCode.HU, Arrays.asList("HU"));
        mapping.put(CountryCode.IL, Arrays.asList("IL"));
        mapping.put(CountryCode.IT, Arrays.asList("IT"));
        mapping.put(CountryCode.JP, Arrays.asList("JP"));
        mapping.put(CountryCode.LU, Arrays.asList("LU"));
        mapping.put(CountryCode.MT, Arrays.asList("MT"));
        mapping.put(CountryCode.NL, Arrays.asList("NL"));
        mapping.put(CountryCode.NO, Arrays.asList("NO"));
        mapping.put(CountryCode.PL, Arrays.asList("PL"));
        mapping.put(CountryCode.RO, Arrays.asList("RO"));
        mapping.put(CountryCode.RS, Arrays.asList("RS"));
        mapping.put(CountryCode.SE, Arrays.asList("SE"));
        mapping.put(CountryCode.SI, Arrays.asList("SI"));
        mapping.put(CountryCode.SK, Arrays.asList("SK"));
        mapping.put(CountryCode.TR, Arrays.asList("TR"));
        mapping.put(CountryCode.UA, Arrays.asList("UA"));
        mapping.put(CountryCode.GB, Arrays.asList("UK"));
        mapping.put(CountryCode.UM, Arrays.asList("UM"));
        mapping.put(CountryCode.US, Arrays.asList("US"));

        return mapping;
    }

    /**
     * @return Form type mapping.
     */
    private Map<Enum, List<String>> formTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(PublicationFormType.CONTRACT_NOTICE, Arrays.asList("Eljárást megindító felhívás - 121. § (1) " +
                        "bekezdés b) pontja/KÉ/2013.07.01 KÉ", "Eljárást megindító felhívás", "Ajánlati " +
                        "felhívás/EU/2011.08.19. EUHL", "Ajánlati felhívás", "Ajánlati/Részvételi felhívás/2015 EUHL",
                "Ajánlati/Részvételi felhívás", "Részvételi felhívás - Egyes ágazatokban/EU/2011.08.19. EUHL",
                "Részvételi felhívás", "Tájékoztató a koncessziós eljárás eredményéről/2015 KÉ",
                "Koncessziós hirdetmény"));
        mapping.put(PublicationFormType.CONTRACT_AWARD, Arrays.asList("Tájékoztató az eljárás eredményéről (1-es " +
                        "minta)/KÉ/2013.07.01 KÉ", "Tájékoztató az eljárás eredményéről (1-es minta)/KÉ/2011.12.30 KÉ",
                "Tájékoztató az eljárás eredményéről", "Tájékoztató az eljárás eredményéről/2015 KÉ",
                "Szociális és egyéb " +
                        "meghatározott " +
                        "szolgáltatások – Általános közbeszerzés/2015 EUHL",
                "Szociális és egyéb meghatározott szolgáltatások"));
        mapping.put(PublicationFormType.CONTRACT_CANCELLATION, Arrays.asList("tájékoztató a szerződés teljesítéséről_" +
                " KÉ", "tájékoztató a szerződés teljesítéséről KÉ"));
        mapping.put(PublicationFormType.CONTRACT_UPDATE, Arrays.asList("Tájékoztató a szerződés " +
                        "módosításáról/KÉ/2013.07.01 KÉ", "Tájékoztató a szerződés módosításáról", "További " +
                        "információ, " +
                        "befejezetlen eljárás vagy korrigendum/EU/2011.12.30 EUHL", "További információ, befejezetlen" +
                        " eljárás" +
                        " vagy korrigendum", "Tájékoztató a hirdetmény visszavonásáról, módosításáról/KÉ/2011.12.30 KÉ",
                "Tájékoztató a hirdetmény visszavonásáról, módosításáról", "Helyesbítés/2015 EUHL",
                "Módosítás/helyesbítés/visszavonás/2015 KÉ"));
        mapping.put(PublicationFormType.PRIOR_INFORMATION_NOTICE, Arrays.asList("Előzetes tájékoztató/EU/2011.08.19. " +
                "EUHL", "Előzetes tájékoztató"));
        mapping.put(PublicationFormType.OTHER, Arrays.asList("Tervpályázati kiírás/EU/2011.08.19. EUHL",
                "Tervpályázati kiírás", "Tájékoztató a tervpályázati eljárás eredményéről", "Önkéntes előzetes " +
                        "átláthatóságra vonatkozó hirdetmény",
                "Előminősítési rendszer - Egyes ágazatokban/EU/2011.08.19. EUHL",
                "Felhasználói oldalon közzétett hirdetmény/EU/2011.08.19. EUHL",
                "Tájékoztató a koncesszió odaítéléséről/2015 EUHL",
                "Előminősítési hirdetmény – Közszolgáltatások/2015 EUHL"));

        return mapping;
    }
}
