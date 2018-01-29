package eu.digiwhist.worker.nl.clean;

import eu.digiwhist.worker.clean.BaseDigiwhistTenderCleaner;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.BuyerActivityType;
import eu.dl.dataaccess.dto.codetables.BuyerType;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.SelectionMethod;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.codetables.TenderSupplyType;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.AddressPlugin;
import eu.dl.worker.clean.plugin.AwardCriteriaPlugin;
import eu.dl.worker.clean.plugin.BodyPlugin;
import eu.dl.worker.clean.plugin.DatePlugin;
import eu.dl.worker.clean.plugin.DateTimePlugin;
import eu.dl.worker.clean.plugin.FundingsPlugin;
import eu.dl.worker.clean.plugin.IntegerPlugin;
import eu.dl.worker.clean.plugin.LotPlugin;
import eu.dl.worker.clean.plugin.PublicationPlugin;
import eu.dl.worker.clean.plugin.SelectionMethodPlugin;
import eu.dl.worker.clean.plugin.TenderProcedureTypePlugin;
import eu.dl.worker.clean.plugin.TenderSupplyTypePlugin;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;

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
 * Tender cleaner for TenderNed in Netherlands.
 *
 * @author Marek Mikes
 */
public class TenderNedTenderCleaner extends BaseDigiwhistTenderCleaner {
    private static final String VERSION = "1";

    private static final Locale LOCALE = new Locale("nl");

    private static final List<NumberFormat> NUMBER_FORMATS;

    static {
        NUMBER_FORMATS = new ArrayList<>();

        NUMBER_FORMATS.add(NumberFormat.getInstance(LOCALE));

        DecimalFormatSymbols formatSymbols = new DecimalFormatSymbols(LOCALE);
        formatSymbols.setDecimalSeparator(',');
        formatSymbols.setGroupingSeparator(' ');
        NUMBER_FORMATS.add(new DecimalFormat("#,##0.###", formatSymbols));
    }

    private static final List<DateTimeFormatter> DATE_FORMATTERS = Arrays.asList(
            DateTimeFormatter.ofPattern("dd/MM/yyyy"),
            DateTimeFormatter.ofPattern("dd-MM-yyyy"));

    // date time examples:
    //  "09/09/2015 Tijdstip: 16:00"
    //  "05/12/2016 Plaatselijke tijd: 10:00"
    //  "31/10/2016 Plaatselijke tijd: -"
    private static final List<DateTimeFormatter> DATETIME_FORMATTERS = Arrays.asList(
            new DateTimeFormatterBuilder()
                    .appendPattern("dd/MM/yyyy")
                    .appendLiteral(" Tijdstip: ")
                    .appendPattern("HH:mm")
                    .toFormatter(LOCALE),
            new DateTimeFormatterBuilder()
                    .appendPattern("dd/MM/yyyy")
                    .optionalStart()
                    .appendLiteral(" Plaatselijke tijd: ")
                    .optionalEnd()
                    //optional time
                    .optionalStart()
                    .appendPattern("HH:mm")
                    .optionalEnd()
                    //optional not entered time
                    .optionalStart()
                    .appendLiteral("-")
                    .optionalEnd()
                    //default values for time
                    .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
                    .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
                    .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
                    .toFormatter(LOCALE));

    @Override
    public final String getVersion() {
        return VERSION;
    }

    @Override
    protected final ParsedTender preProcessParsedItem(final ParsedTender parsedItem) {
        // remove bid deadline when it has no useful information
        if (parsedItem.getBidDeadline() != null && parsedItem.getBidDeadline().equals("- Tijdstip: -")) {
            parsedItem.setBidDeadline(null);
        }

        // use nationalProcedureType as procedureType
        parsedItem.setProcedureType(parsedItem.getNationalProcedureType());

        return parsedItem;
    }

    @Override
    protected final CleanTender postProcessSourceSpecificRules(final ParsedTender parsedTender,
                                                               final CleanTender cleanTender) {
        return cleanTender;
    }

    @SuppressWarnings("unchecked")
    @Override
    protected final void registerSpecificPlugins() {
        pluginRegistry
                .registerPlugin("integerPlugin", new IntegerPlugin(NUMBER_FORMATS))
                .registerPlugin("supplyType", new TenderSupplyTypePlugin(getSupplyTypeMapping()))
                .registerPlugin("date", new DatePlugin(DATE_FORMATTERS))
                .registerPlugin("datetime", new DateTimePlugin(DATETIME_FORMATTERS))
                .registerPlugin("bodies", new BodyPlugin(getBuyerTypeMapping(), getBodyActivityMapping()))
                .registerPlugin("lots", new LotPlugin(NUMBER_FORMATS, DATE_FORMATTERS, new HashMap<>()))
                .registerPlugin("fundings", new FundingsPlugin(NUMBER_FORMATS))
                .registerPlugin("address", new AddressPlugin())
                .registerPlugin("awardCriteria", new AwardCriteriaPlugin(NUMBER_FORMATS))
                .registerPlugin("publications",
                        new PublicationPlugin(NUMBER_FORMATS, DATE_FORMATTERS, getFormTypeMapping()))
                .registerPlugin("procedureType", new TenderProcedureTypePlugin(procedureTypeMapping()))
                .registerPlugin("selectionMethod", new SelectionMethodPlugin(selectionMethodMapping()));
    }

    /**
     * @return supply type mapping for cleaning process
     */
    private static Map<Enum, List<String>> getSupplyTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderSupplyType.WORKS, Arrays.asList("(a) Werken", "Werken"));
        mapping.put(TenderSupplyType.SUPPLIES, Arrays.asList("(b) Leveringen", "Leveringen"));
        mapping.put(TenderSupplyType.SERVICES, Arrays.asList("(c) Diensten", "Diensten"));

        return mapping;
    }

    /**
     * @return body type mapping
     */
    private static Map<Enum, List<String>> getBuyerTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(BuyerType.PUBLIC_BODY, Arrays.asList("Publiekrechtelijke instelling"));
        mapping.put(BuyerType.NATIONAL_AGENCY, Arrays.asList("Nationaal of federaal agentschap/bureau"));
        mapping.put(BuyerType.NATIONAL_AUTHORITY, Arrays.asList(
                "Ministerie of andere nationale of federale instantie, met regionale of plaatselijke " +
                        "onderverdelingen ervan",
                "Ministerie of andere nationale of federale instantie, met inbegrip van regionale of " +
                        "plaatselijke onderverdelingen"));
        mapping.put(BuyerType.REGIONAL_AGENCY, Arrays.asList("Regionaal of plaatselijk agentschap/bureau"));
        mapping.put(BuyerType.REGIONAL_AUTHORITY, Arrays.asList("Regionale of plaatselijke instantie",
                "Andere: Gemeente", "Andere: gemeente"));
        mapping.put(BuyerType.EUROPEAN_AGENCY, Arrays.asList(
                "Europese instelling/Europees agentschap of internationale organisatie"));

        return mapping;
    }

    /**
     * @return body activities mapping
     */
    private Map<Enum, List<String>> getBodyActivityMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(BuyerActivityType.GENERAL_PUBLIC_SERVICES, Arrays.asList("Algemene overheidsdiensten"));
        mapping.put(BuyerActivityType.EDUCATION, Arrays.asList("Onderwijs", "Onderwijs Andere: Onderzoek",
                "Onderwijs Andere: onderzoek"));
        mapping.put(BuyerActivityType.PUBLIC_ORDER_AND_SAFETY, Arrays.asList("Openbare orde en veiligheid"));
        mapping.put(BuyerActivityType.HEALTH, Arrays.asList("Gezondheid"));
        mapping.put(BuyerActivityType.HOUSING_AND_COMMUNITY_AMENITIES, Arrays.asList(
                "Huisvesting en gemeenschappelijke voorzieningen"));
        mapping.put(BuyerActivityType.DEFENCE, Arrays.asList("Defensie"));
        mapping.put(BuyerActivityType.ENVIRONMENT, Arrays.asList("Milieu"));
        mapping.put(BuyerActivityType.ECONOMIC_AND_FINANCIAL_AFFAIRS, Arrays.asList("Economische en financiële zaken"));
        mapping.put(BuyerActivityType.RECREATION_CULTURE_AND_RELIGION, Arrays.asList(
                "Recreatie, cultuur en godsdienst"));
        mapping.put(BuyerActivityType.SOCIAL_PROTECTION, Arrays.asList("Sociale bescherming"));
        mapping.put(BuyerActivityType.WATER, Arrays.asList("Andere: Waterschap",
                "Algemene overheidsdiensten Andere: Waterschap", "Andere: Waterschap / drinkwater"));

        return mapping;
    }

    /**
     * @return form type mapping
     */
    private Map<Enum, List<String>> getFormTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(PublicationFormType.CONTRACT_NOTICE, Arrays.asList(
                "Aankondiging van een opdracht", "Aankondiging van een opdracht - Nutssectoren",
                "Vereenvoudigde aankondiging van een overheidsopdracht in het kader van een dynamisch aankoopsysteem",
                "Aankondiging van een prijsvraag voor ontwerpen",
                "Aankondiging van een opdracht - Overheidsopdrachten - Sociale en andere specifieke diensten",
                "Aankondiging van een opdracht voor opdrachten op het gebied van defensie en veiligheid",
                "Aankondiging van een opdracht - opdrachten gegund door een concessiehouder die zelf geen " +
                        "aanbestedende dienst is"));
        mapping.put(PublicationFormType.CONTRACT_AWARD, Arrays.asList(
                "Aankondiging van een gegunde opdracht", "Aankondiging van een gegunde opdracht - Nutssectoren",
                "Aankondiging van een gegunde opdracht - Overheidsopdrachten - Sociale en andere specifieke diensten",
                "Resultaten van prijsvraag voor ontwerpen"));
        mapping.put(PublicationFormType.PRIOR_INFORMATION_NOTICE, Arrays.asList(
                "Vooraankondiging", "Periodieke indicatieve aankondiging - Nutssectoren",
                "Vooraankondiging - Overheidsopdrachten - Sociale en andere specifieke diensten",
                "Vooraankondiging voor opdrachten op het gebied van defensie en veiligheid"));
        mapping.put(PublicationFormType.CONTRACT_UPDATE, Arrays.asList(
                "Kennisgeving van aanvullende informatie, informatie over een onvolledige procedure of rectificatie"));
        mapping.put(PublicationFormType.OTHER, Arrays.asList(
                "Aankondiging in het geval van vrijwillige transparantie vooraf", "Erkenningsregeling - Nutssectoren",
                "Marktconsultatie", "Aankondiging door middel van een kopersprofiel",
                "Aankondiging van een concessieovereenkomst", "Concessieovereenkomst voor openbare werken",
                "Aankondiging van de gunning van een concessieovereenkomst"));

        return mapping;
    }

    /**
     * @return procedure type mapping
     */
    private Map<Enum, List<String>> procedureTypeMapping() {
        Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderProcedureType.COMPETITIVE_DIALOG, Arrays.asList("Concurrentiegerichte dialoog"));
        mapping.put(TenderProcedureType.OUTRIGHT_AWARD, Arrays.asList(
            "Gunning van een opdracht zonder voorafgaande bekendmaking van een aankondiging van de opdracht in het"
                + " Publicatieblad van de Europese Unie (in de gevallen vermeld in afdeling 2 van bijlage D1)",
            "Gunning van een opdracht zonder voorafgaande bekendmaking van een aankondiging van de opdracht in het"
                + " Publicatieblad van de Europese Unie (in de gevallen vermeld onder k) en l) in bijlage D)",
            "Gunning van een opdracht zonder voorafgaande bekendmaking van een oproep tot mededinging in het"
                + " Publicatieblad van de Europese Unie in onderstaande gevallen"));
        mapping.put(TenderProcedureType.INOVATION_PARTNERSHIP, Arrays.asList("Innovatiepartnerschap"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITH_PUBLICATION, Arrays.asList("Mededingingsprocedure met"
            + " onderhandeling", "Onderhandelingsprocedure met een oproep tot mededinging"));
        mapping.put(TenderProcedureType.RESTRICTED, Arrays.asList("Niet-openbaar", "Niet-openbare procedure",
            "Versneld niet-openbaar"));
        mapping.put(TenderProcedureType.NEGOTIATED, Arrays.asList("Onderhandeling",
            "Versnelde onderhandelingsprocedure"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITHOUT_PUBLICATION, Arrays.asList("Onderhandelingsprocedure zonder"
            + " bekendmaking van een aankondiging van een opdracht/een oproep tot mededinging",
            "Onderhandelingsprocedure zonder een oproep tot mededinging"));
        mapping.put(TenderProcedureType.OPEN, Arrays.asList("Openbaar", "Openbare procedure"));

        return mapping;

    }

    /**
     * @return selection method mapping
     */
    private Map<Enum, List<String>> selectionMethodMapping() {
        Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(SelectionMethod.MEAT, Arrays.asList("Economisch meest voordelige inschrijving, gelet op"));
        mapping.put(SelectionMethod.LOWEST_PRICE, Arrays.asList("Laagste prijs"));
                
        return mapping;
    }
}
