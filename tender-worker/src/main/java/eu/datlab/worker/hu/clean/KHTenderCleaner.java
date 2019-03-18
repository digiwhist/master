package eu.datlab.worker.hu.clean;

import eu.datlab.worker.clean.BaseDatlabTenderCleaner;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.BuyerActivityType;
import eu.dl.dataaccess.dto.codetables.BuyerType;
import eu.dl.dataaccess.dto.codetables.CountryCode;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.SelectionMethod;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.codetables.TenderSupplyType;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedFunding;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
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

import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.temporal.ChronoField;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Created by michalriha on 26/04/2017.
 */
public class KHTenderCleaner extends BaseDatlabTenderCleaner {
    private static final String VERSION = "1.0";

    private static final NumberFormat NUMBER_FORMAT = NumberFormat.getInstance(Locale.GERMAN);

    private static final List<DateTimeFormatter> DATE_FORMATTERS = Arrays.asList(
            DateTimeFormatter.ofPattern("uuuu.MM.dd"),
            DateTimeFormatter.ofPattern("uuuu.MM.dd."),
            DateTimeFormatter.ofPattern("uuuu/MM/dd"));

    private static final List<DateTimeFormatter> DATETIME_FORMATTERS = Arrays.asList(
        DateTimeFormatter.ISO_LOCAL_DATE_TIME, DateTimeFormatter.ISO_OFFSET_DATE_TIME,
        new DateTimeFormatterBuilder().appendPattern("yyyy/MM/dd[ HH:mm:ss]")
            .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
            .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
            .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
            .toFormatter(),
        new DateTimeFormatterBuilder().appendPattern("yyyy.MM.dd.[ HH:mm:ss]")
            .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
            .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
            .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
            .toFormatter());

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
                        new PublicationPlugin(NUMBER_FORMAT, DATE_FORMATTERS, formTypeMapping()))
                .registerPlugin("lots", new LotPlugin(NUMBER_FORMAT, DATE_FORMATTERS, lotMappings))
                .registerPlugin("prices", new PricePlugin(NUMBER_FORMAT))
                .registerPlugin("address", new AddressPlugin())
                .registerPlugin("bodies", new BodyPlugin(buyerTypeMapping(), activityMapping(), countryMapping()))
                .registerPlugin("procedureType",
                        new TenderProcedureTypePlugin(procedureTypeMapping(), acceleratedProcedures()))
                .registerPlugin("selectionMethod", new SelectionMethodPlugin(selectionMethodMapping()))
                .registerPlugin("fundings", new FundingsPlugin(NUMBER_FORMAT))
                .registerPlugin("awardCriteria", new AwardCriteriaPlugin(NUMBER_FORMAT))
                .registerPlugin("supplyType", new TenderSupplyTypePlugin(supplyTypeMapping()));
    }

    /**
     * @return selection Method mapping for cleaning process
     */
    private Map<Enum, List<String>> selectionMethodMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(SelectionMethod.MEAT, Arrays.asList("Az összességében legelőnyösebb ajánlat az alábbiak szerint",
                "1 2. Jótállás időtartama (120 hónap kötelező jótálláson felül maximum további 60 hónap) 30 2 3. Műszaki" +
                        " és pénzügyi ütemterv kidolgozottsága 20"));
        mapping.put(SelectionMethod.LOWEST_PRICE, Arrays.asList("Ár – Súlyszám:", "A legalacsonyabb összegű ellenszolgáltatás"));
        return mapping;
    }

    /**
     * @return buyer activity type mapping.
     */
    private Map<Enum, List<String>> activityMapping() {
        Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(BuyerActivityType.OTHER, Arrays.asList(
                "elektronikus közigazgatási informatikai és távközlési közszolgáltatás"));
        mapping.put(BuyerActivityType.HEALTH, Arrays.asList("Egészségügy"));
        mapping.put(BuyerActivityType.URBAN_TRANSPORT,
                Arrays.asList("Városi vasúti, villamos-, trolibusz- és autóbusz-szolgáltatások"));


        return mapping;
    }

    /**
     * @return buyer type mapping
     */
    private Map<Enum, List<String>> buyerTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(BuyerType.PUBLIC_BODY, Arrays.asList("Közjogi intézmény"));
        mapping.put(BuyerType.REGIONAL_AGENCY, Arrays.asList("Regionális/helyi szintű"));

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
        parsedItem.setIsCoveredByGpa(toBooleanString(parsedItem.getIsCoveredByGpa()));
        parsedItem.setHasLots(toBooleanString(parsedItem.getHasLots()));
        parsedItem.setHasOptions(toBooleanString(parsedItem.getHasOptions()));
        parsedItem.setAreVariantsAccepted(toBooleanString(parsedItem.getAreVariantsAccepted()));
        parsedItem.setIsAwarded(toBooleanString(parsedItem.getIsAwarded()));
        parsedItem.setIsElectronicAuction(toBooleanString(parsedItem.getIsElectronicAuction()));
        if (parsedItem.getFundings() != null && !parsedItem.getFundings().isEmpty()) {
            for (ParsedFunding funding : parsedItem.getFundings()) {
                funding.setIsEuFund(toBooleanString(funding.getIsEuFund()));
            }
        }

        if (parsedItem.getLots() != null) {
            for (ParsedTenderLot lot : parsedItem.getLots()) {
                if (lot.getBids() != null) {
                    for (ParsedBid bid : lot.getBids()) {
                        bid.setIsSubcontracted(toBooleanString(bid.getIsSubcontracted()));
                        bid.setIsConsortium(toBooleanString(bid.getIsConsortium()));
                    }
                }
            }
        }

        return parsedItem;
    }

    /**
     * @return supply type mapping for cleaning process
     */
    private static Map<Enum, List<String>> supplyTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(TenderSupplyType.SERVICES, Arrays.asList("Szolgáltatásmegrendelés", "Szolgáltatás megrendelés", "Szolgáltatás"));
        mapping.put(TenderSupplyType.WORKS, Arrays.asList("Építési beruházás", "Építési beruházás Kivitelezés",
                "Építési beruházás Tervezés és kivitelezés",
                "Eljárást megindító felhívás Közbeszerzési Értesítőben történt közzététele nélkül odaítélt szerződés az alább felsor" +
                        "olt esetekben A Kbt. 115. § szerinti nyílt eljárás"));
        mapping.put(TenderSupplyType.SUPPLIES, Arrays.asList("Árubeszerzés", "Árubeszerzés Adásvétel"));


        return mapping;
    }

    /**
     * @return accelerated procedures
     */
    private List<String> acceleratedProcedures() {
        return new ArrayList<>(Arrays.asList("Gyorsított tárgyalásos", "Gyorsított meghívásos", "Nyílt eljárás Gyorsított eljárás",
            "Nyílt eljárás Gyorsított eljárás (klasszikus ajánlatkérők esetében)"));
    }

    /**
     * @return procedure type mapping for cleaning process
     */
    private static Map<Enum, List<String>> procedureTypeMapping() {
        
        //A Kbt. 117. § szerinti saját beszerzési szabályok szerinti eljárás
        //Tárgyalásokat is magában foglaló eljárás
        //Tárgyalásos eljárás Gyorsított eljárás       
        //Meghívásos eljárás Gyorsított eljárás
        //Koncessziós beszerzési eljárás koncessziós hirdetmény előzetes közzétételével

        final Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(TenderProcedureType.OPEN, Arrays.asList("Nyílt", "Kbt. 122/A. § szerinti eljárás", "A Kbt. 113. § szerinti nyílt"
            + " eljárás", "A Kbt. 115. § szerinti nyílt eljárás", "Nyílt eljárás", "A Kbt. 115. § szerinti nyílt eljárás", "A Kbt. 113. §"
            + " szerinti nyílt eljárás", "Nyílt eljárás", "Nyílt eljárás Gyorsított eljárás", "Nyílt eljárás Gyorsított eljárás (klasszikus"
            + " ajánlatkérők esetében)"));
        mapping.put(TenderProcedureType.RESTRICTED, Arrays.asList("A Kbt. 113. § szerinti meghívásos eljárás", "Meghívásos", "Meghívásos"
            + " eljárás"));
        mapping.put(TenderProcedureType.COMPETITIVE_DIALOG, Arrays.asList("Versenypárbeszéd"));
        mapping.put(TenderProcedureType.NEGOTIATED, Arrays.asList("Tárgyalásos", "Felhívással induló tárgyalásos eljárás", "Gyorsított"
            + " tárgyalásos", "Tárgyalásos eljárás", "Hirdetménnyel induló, tárgyalásos"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITH_PUBLICATION, Arrays.asList("Hirdetmény közzétételével induló tárgyalásos",
            "Ajánlati/részvételi felhívás közzététele nélküli/hirdetmény nélküli tárgyalásos"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITHOUT_PUBLICATION, Arrays.asList("A Kbt. 115. § szerinti hirdetmény nélküli"
            + " tárgyalásos eljárás", "Hirdetmény nélküli tárgyalásos", "Eljárást megindító felhívás Közbeszerzési Értesítőben történt"
            + " közzététele nélkül odaítélt szerződés az alább felsorolt esetekben A Kbt. 115. § szerinti nyílt eljárás", "Eljárást"
            + " megindító felhívás Közbeszerzési Értesítőben történt közzététele nélkül odaítélt szerződés az alább felsorolt esetekben"
            + " A Kbt. 113. § szerinti nyílt eljárás", "Eljárást megindító felhívás Közbeszerzési Értesítőben történt közzététele nélkül"
            + " odaítélt szerződés az alább felsorolt esetekben A Kbt. 115. § szerinti hirdetmény nélküli tárgyalásos eljárás",
            "Ajánlati/részvételi felhívásnak az Európai Unió Hivatalos Lapjában történő közzététele nélkül megvalósított beszerzés",
            "Előzetes közzététel nélküli tárgyalásos eljárás", "Eljárást megindító felhívás Közbeszerzési Értesítőben történt közzététele"
            + " nélkül odaítélt szerződés az alább felsorolt esetekben Hirdetmény nélküli tárgyalásos eljárás", "Eljárást megindító"
            + " felhívás Közbeszerzési Értesítőben történt közzététele nélkül odaítélt szerződés az alább felsorolt esetekben A Kbt. 113. §"
            + " szerinti tárgyalásos eljárás", "Eljárást megindító felhívásnak a Közbeszerzési Értesítőben történő közzététele nélkül"
            + " odaítélt szerződés", "Eljárást megindító felhívásnak az Európai Unió Hivatalos Lapjában történő közzététele nélkül odaítélt"
            + " szerződés az alább felsorolt esetekben (töltse ki a D1. mellékletet)", "Eljárást megindító felhívás Közbeszerzési"
            + " Értesítőben történt közzététele nélkül odaítélt szerződés az alább felsorolt esetekben A Kbt. 113. § szerinti meghívásos"
            + " eljárás", "Eljárást megindító felhívásnak az Európai Unió Hivatalos Lapjában történő közzététele nélkül odaítélt szerződés"
            + " az alább felsorolt esetekben (töltse ki a D2. mellékletet)"));
        mapping.put(TenderProcedureType.DESIGN_CONTEST, Arrays.asList());
        mapping.put(TenderProcedureType.OUTRIGHT_AWARD, Arrays.asList());
        mapping.put(TenderProcedureType.APPROACHING_BIDDERS, Arrays.asList("Gyorsított meghívásos"));

        return mapping;
    }

    /**
     * @return country mapping
     */
    private Map<Enum, List<String>> countryMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(CountryCode.HU, Arrays.asList("HU", "Magyarorszag", "Magyarország", "Magyar"));
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
                "Koncessziós hirdetmény", "Részvételi felhívás/EU/2011.08.19. EUHL",
                "Tájékoztató az eljárás eredményéről – Közszolgáltatások/2015 EUHL"));
        mapping.put(PublicationFormType.CONTRACT_AWARD, Arrays.asList("Tájékoztató az eljárás eredményéről (1-es " +
                        "minta)/KÉ/2013.07.01 KÉ", "Tájékoztató az eljárás eredményéről (1-es minta)/KÉ/2011.12.30 KÉ",
                "Tájékoztató az eljárás eredményéről", "Tájékoztató az eljárás eredményéről/2015 KÉ",
                "Szociális és egyéb meghatározott szolgáltatások – Általános közbeszerzés/2015 EUHL",
                "Szociális és egyéb meghatározott szolgáltatások", "Tájékoztató az eljárás " +
                        "eredményéről/EU/2011.08.19. EUHL", "Tájékoztató az eljárás eredményéről/2015 EUHL"));
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
                "EUHL", "Előzetes tájékoztató", "A hirdetmény kizárólag előzetes tájékoztatás céljára szolgál"));
        mapping.put(PublicationFormType.OTHER, Arrays.asList("Tervpályázati kiírás/EU/2011.08.19. EUHL",
                "Tervpályázati kiírás", "Tájékoztató a tervpályázati eljárás eredményéről", "Önkéntes előzetes " +
                        "átláthatóságra vonatkozó hirdetmény",
                "Előminősítési rendszer - Egyes ágazatokban/EU/2011.08.19. EUHL",
                "Felhasználói oldalon közzétett hirdetmény/EU/2011.08.19. EUHL",
                "Tájékoztató a koncesszió odaítéléséről/2015 EUHL",
                "Előminősítési hirdetmény – Közszolgáltatások/2015 EUHL"));

        return mapping;
    }

    /**
     * Create boolean string from hungarian yes and no.
     * @param input input
     * @return String or null
     */
    private String toBooleanString(final String input) {
        if (input == null) {
            return null;
        }

        if (input.toLowerCase().contains("igen")) {
            return Boolean.TRUE.toString();
        } else if (input.toLowerCase().contains("nem")) {
            return Boolean.FALSE.toString();
        } else {
            return null;
        }
    }
}
