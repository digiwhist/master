package eu.datlab.worker.ee.clean;

import eu.datlab.worker.clean.BaseDatlabTenderCleaner;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.BuyerActivityType;
import eu.dl.dataaccess.dto.codetables.BuyerType;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.SelectionMethod;
import eu.dl.dataaccess.dto.codetables.TenderLotStatus;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.codetables.TenderSize;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
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
import eu.dl.worker.clean.plugin.TenderSizePlugin;
import eu.dl.worker.clean.utils.CodeTableUtils;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.temporal.ChronoField;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;


/**
 * Tender crawler for E-procurement in Estonia.
 *
 * @author Marek Mikes
 */
public final class EPETenderCleaner extends BaseDatlabTenderCleaner {
    private static final String VERSION = "1.0";

    private static final Locale LOCALE = new Locale("ee");

    private static final DateTimeFormatter DATETIME_FORMATTER = new DateTimeFormatterBuilder()
        .appendPattern("dd.MM.uuuu[ H:m]")
        .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
        .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
        .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
        .toFormatter(LOCALE);

    private static final List<NumberFormat> NUMBER_FORMATS = new ArrayList<>();
    // number format with comma as decimal separator
    static {
        DecimalFormatSymbols formatSymbols = new DecimalFormatSymbols(LOCALE);
        formatSymbols.setDecimalSeparator(',');
        formatSymbols.setGroupingSeparator(' ');
        NUMBER_FORMATS.add(new DecimalFormat("#,##0.###", formatSymbols));
    }
    // number format with dot as decimal separator
    static {
        DecimalFormatSymbols formatSymbols = new DecimalFormatSymbols(LOCALE);
        formatSymbols.setDecimalSeparator('.');
        formatSymbols.setGroupingSeparator(' ');
        NUMBER_FORMATS.add(new DecimalFormat("#,##0.###", formatSymbols));
    }
    

    @Override
    protected void registerSpecificPlugins() {
        pluginRegistry
            .registerPlugin("body", new BodyPlugin(buyerTypeMapping(), buyerActivityMapping()))
            .registerPlugin("lot", new LotPlugin(NUMBER_FORMATS, Collections.singletonList(DATETIME_FORMATTER),
                lotMappings()))
            .registerPlugin("publication", new PublicationPlugin(NUMBER_FORMATS,
                Collections.singletonList(DATETIME_FORMATTER), formTypeMapping()))
            .registerPlugin("date", new DatePlugin(DATETIME_FORMATTER))
            .registerPlugin("datetime", new DateTimePlugin(DATETIME_FORMATTER))
            .registerPlugin("selectionMethod", new SelectionMethodPlugin(selectionMethodMapping()))
            .registerPlugin("procedureType", new TenderProcedureTypePlugin(procedureTypeMapping()))
            .registerPlugin("price", new PricePlugin(NUMBER_FORMATS))
            .registerPlugin("fundings", new FundingsPlugin(NUMBER_FORMATS))
            .registerPlugin("address", new AddressPlugin())
            .registerPlugin("integers", new IntegerPlugin(NUMBER_FORMATS))
            .registerPlugin("awardCriteria", new AwardCriteriaPlugin(NUMBER_FORMATS))
            .registerPlugin("tenderSize", new TenderSizePlugin(CodeTableUtils.enumToMapping(TenderSize.class)));
    }

    /**
     * @return procedure type mapping
     */
    private static Map<Enum, List<String>> procedureTypeMapping() {
        Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderProcedureType.OPEN, Arrays.asList("Avatud hankemenetlus", "Lihthange"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITHOUT_PUBLICATION, Arrays.asList("Väljakuulutamiseta läbirääkimistega hankemenetlus"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITH_PUBLICATION, Arrays.asList("Väljakuulutamisega läbirääkimistega hankemenetlus"));
        mapping.put(TenderProcedureType.DESIGN_CONTEST, Arrays.asList("Avatud ideekonkurss", "Piiratud osalejate arvuga ideekonkurss"));
        mapping.put(TenderProcedureType.COMPETITIVE_DIALOG, Arrays.asList("Võistlev dialoog"));
        mapping.put(TenderProcedureType.MINITENDER, Arrays.asList("Minikonkurss raamlepingu alusel"));
        mapping.put(TenderProcedureType.RESTRICTED, Arrays.asList("Piiratud hankemenetlus"));
        mapping.put(TenderProcedureType.CONCESSION, Arrays.asList("Ehitustööde kontsessioon", "Kontsessioon"));
        mapping.put(TenderProcedureType.OTHER, Arrays.asList("Lihtsustatud korras teenuste tellimine", "Erand § 14", "Vabatahtlik teade"));
        mapping.put(TenderProcedureType.OUTRIGHT_AWARD, Arrays.asList("Vabatahtlik teade - leping ilma hanketeate eelneva avaldamis"));
        mapping.put(TenderProcedureType.PUBLIC_CONTEST, Arrays.asList("Konkurentsipõhine läbirääkimistega hankemenetlus"));
        mapping.put(TenderProcedureType.INOVATION_PARTNERSHIP, Arrays.asList("Innovatsioonipartnerlus"));

        return mapping;
    }

    /**
     * @return selection method mapping
     */
    private static Map<Enum, List<String>> selectionMethodMapping() {
        Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(SelectionMethod.LOWEST_PRICE, Collections.singletonList("Madalaim hind"));
        mapping.put(SelectionMethod.MEAT, Arrays.asList("Majanduslikult soodsaim pakkumus",
            "Parima hinna ja kvaliteedi suhe"));

        return mapping;
    }

    /**
     * @return lot status mapping
     */
    private static Map<Enum, List<String>> statusMapping() {
        Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderLotStatus.CANCELLED, Collections.singletonList("CANCELED"));
        return mapping;
    }

    /**
     * @return form type mapping
     */
    private Map<Enum, List<String>> formTypeMapping() {
        Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(PublicationFormType.PRIOR_INFORMATION_NOTICE, Arrays.asList("Eelteade",
            "Eelteade - kaitse- ja julgeolekuvaldkond"));
        mapping.put(PublicationFormType.CONTRACT_NOTICE, Arrays.asList("Hanketeade", "Hanketeade (Võrgustik)",
            "Hanketeade - kaitse- ja julgeolekuvaldkond"));
        mapping.put(PublicationFormType.CONTRACT_AWARD, Arrays.asList("Riigihanke aruanne",
            "Ehitustööde kontsessiooni teade", "Ideekonkursi tulemused"));
        mapping.put(PublicationFormType.CONTRACT_AMENDMENT, Arrays.asList("Riigihanke aruande lisa"));

        /*
        Kvalifitseerimissüsteem (Võrgustiku) - Qualification system (network)
        Dünaamilise hankesüsteemi lihtsustatud hanketeade - Simplified contract notice on a dynamic purchasing system
        Perioodiline eelteade (VS)
        Ideekonkursi kutse - Design contest notice
        Vabatahtlik eelnev avalikustamisteade - Voluntary ex ante transparency
        Perioodiline eelteade tähtaja lühendamiseks (VS)
        Hanketeade ehitustööde hankelepingute kohta
        */

        return mapping;
    }

    /**
     * @return lot mappings
     */
    private Map<String, Map<Enum, List<String>>> lotMappings() {
         Map<String, Map<Enum, List<String>>> mapping = new HashMap<>();

         mapping.put("selectionMethodMapping", selectionMethodMapping());
         mapping.put("statusMapping", statusMapping());

        return mapping;
    }

    /**
     * @return buyer type mapping
     */
    private Map<Enum, List<String>> buyerTypeMapping() {
        Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(BuyerType.REGIONAL_AUTHORITY, Arrays.asList("Kohaliku omavalitsuse üksus"));
        mapping.put(BuyerType.REGIONAL_AGENCY, Arrays.asList("Kohaliku omavalitsuse asutus"));
        mapping.put(BuyerType.NATIONAL_AGENCY, Arrays.asList("Riigiasutus või valitsusasutus",
            "Valitsusasutuse hallatav riigiasutus"));
        mapping.put(BuyerType.PUBLIC_BODY, Arrays.asList("Avalik-õiguslik juriidiline isik või riigi kontrolli all olev"
            + " eraõiguslik juriidiline isik"));

        /*
        Eraõiguslik - private
        Avalik-õiguslik juriidiline isik või riigi kontrolli all olev eraõiguslik juriidiline isik
            - A public entity controlled by the State or a private entity
        Sihtasutus
        mittetulundusühing, hankija RHS §10 lg2 p1 mõistes - non-profit organization, vendor in terms of the RHS §10
            LG2 p1
        Riigi poolt asutatud sihtasutus
        */

        return mapping;
    }

    /**
     * @return buyer activity mapping
     */
    private Map<Enum, List<String>> buyerActivityMapping() {
        Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(BuyerActivityType.PORT, Arrays.asList("Sadamaga seotud teenused"));
        mapping.put(BuyerActivityType.RECREATION_CULTURE_AND_RELIGION, Arrays.asList("Vaba aeg, kultuur ja religioon"));
        mapping.put(BuyerActivityType.COAL_AND_OTHER_EXTRACTION, Arrays.asList("Söe ja muude tahkekütuste leiukohtade"
            + " uurimine ja kaevandamine"));
        mapping.put(BuyerActivityType.ENVIRONMENT, Arrays.asList("Keskkond", "maaparandus"));
        mapping.put(BuyerActivityType.PUBLIC_ORDER_AND_SAFETY, Arrays.asList("Avalik kord ja julgeolek"));
        mapping.put(BuyerActivityType.EDUCATION, Arrays.asList("Haridus"));
        mapping.put(BuyerActivityType.GENERAL_PUBLIC_SERVICES, Arrays.asList("Üldised avalikud teenused"));
        mapping.put(BuyerActivityType.HEALTH, Arrays.asList("tervisekaitse", "Tervishoid"));
        mapping.put(BuyerActivityType.AIRPORT, Arrays.asList("Lennujaamaga seotud teenused"));
        mapping.put(BuyerActivityType.ECONOMIC_AND_FINANCIAL_AFFAIRS, Arrays.asList("ettevõtete toetamine ja kinnisvara"
            + " rent", "riiklikud tagatised, toetused", "Majandus ja rahandus"));
        mapping.put(BuyerActivityType.HOUSING_AND_COMMUNITY_AMENITIES, Arrays.asList("Kinnisvara", "arendusprojektide"
            + " juhtimine", "Linnaplaneerimine", "ehituse- ja ehitusmaterjalide alased"
            + " konsultatsioonid ja teenused", "Elamu- ja kommunaalmajandus", "Kinnisvara arendus ja haldus"));
        mapping.put(BuyerActivityType.ELECTRICITY, Arrays.asList("Elektrienergia"));
        mapping.put(BuyerActivityType.RAILWAY, Arrays.asList("Raudteeteenused"));
        mapping.put(BuyerActivityType.WATER, Arrays.asList("Vesi"));
        mapping.put(BuyerActivityType.URBAN_TRANSPORT, Arrays.asList("ühistransport", "Linnarongi-, trammi-,"
            + " trollibussi- või bussiteenused"));
        mapping.put(BuyerActivityType.DEFENCE, Arrays.asList("Riigikaitse"));
        mapping.put(BuyerActivityType.GAS_AND_HEAT_PRODUCTION, Arrays.asList("Gaasi ja soojusenergia tootmine,"
            + " transport ja jaotamine"));
        mapping.put(BuyerActivityType.SOCIAL_PROTECTION, Arrays.asList("Sotsiaalkaitse"));
        mapping.put(BuyerActivityType.OTHER, Arrays.asList("Metsamajandus", "IT ja arendus", "Ehitiste haldamine",
            "Teede hoid ja liiklusohutus"));

        /*
        
        erametsandusele suunatud riiklike toetuste ja välisabi programmide rakendamine - private forestry on state
            aid and the implementation of external assistance programs
        välispoliitika teostamine - external exercise
        Teede hooldus - Road maintenance
        maakorraldus - readjustment
        Riigikogule tema põhiseaduslike funktsioonide täitmiseks vajalike tingimuste loomine - Parliament of its
            constitutional functions required for the creation of conditions
        Maanteehoid - Maanteehoid
        telefoni- andme ja raadioside - telephone, data and wireless
        Põllumajandus ja kalandus - Agriculture and Fisheries
        ringhäälingu organisatsioon - broadcaster
        teadus - science
        Teehoid - Road maintenance
        Kohtuekspertiiside tegemine, ekspertiisitegevust toetav teadus- ja arendustegevus - Court Making expert,
            expertise, research and development activities in support of
        Täitev- ja seadusandlike organite tegevus  - Executive and legislative administration
        pääste - salvation
        Ärikeskkonna arendamine - development of the Business Environment
        õigus- ja kriminaalpoliitika - legal and criminal policy        
        põhiseadusliku institutsiooni teenindamine - constitutional institution services
        õigusemõistmine - administration of justice
        infotehnoloogiliste e-tervise projektidega tegelemine - information technology e-health projects addressing
         */

        return mapping;
    }

    @Override
    public String getVersion() {
        return VERSION;
    }

    @Override
    protected CleanTender postProcessSourceSpecificRules(final ParsedTender parsedItem, final CleanTender cleanItem) {
        return cleanItem;
    }

    @Override
    protected ParsedTender preProcessParsedItem(final ParsedTender parsedItem) {
        return parsedItem;
    }
}
