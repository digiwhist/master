package eu.digiwhist.worker.si.clean;

import eu.digiwhist.worker.clean.BaseDigiwhistTenderCleaner;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.BuyerActivityType;
import eu.dl.dataaccess.dto.codetables.BuyerType;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.SelectionMethod;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.codetables.TenderSupplyType;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.BodyPlugin;
import eu.dl.worker.clean.plugin.DatePlugin;
import eu.dl.worker.clean.plugin.DateTimePlugin;
import eu.dl.worker.clean.plugin.FundingsPlugin;
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
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Tender cleaner for E-narocanje in Slovenia.
 *
 * @author Marek Mikes
 */
public class ENarocanjeTenderCleaner extends BaseDigiwhistTenderCleaner {
    private static final String VERSION = "1";

    private static final Locale LOCALE = new Locale("sl");

    private static final NumberFormat NUMBER_FORMAT = NumberFormat.getInstance(LOCALE);

    // date examples:
    //  "04.07.2017"
    //  "6. 8. 2014"
    private static final List<DateTimeFormatter> DATE_FORMATTERS = Arrays.asList(
            DateTimeFormatter.ofPattern("dd.MM.yyyy"),
            DateTimeFormatter.ofPattern("d. M. yyyy"));

    // date time examples:
    //  "20.01.2017   10:00"
    //  "Datum: 13. 9. 2007"
    //  "Datum: 27. 8. 2007 Čas: 12:00"
    //  "Datum: 9. 6. 2009 Čas: 9.:00" (only in https://www.enarocanje.si/Obrazci/?id_obrazec=31335)
    private static final List<DateTimeFormatter> DATETIME_FORMATTERS = Arrays.asList(
            new DateTimeFormatterBuilder()
                    .appendPattern("dd.MM.yyyy   HH:mm")
                    .toFormatter(LOCALE),
            new DateTimeFormatterBuilder()
                    .appendLiteral("Datum: ")
                    .appendPattern("d. M. yyyy")
                    //optional time
                    .optionalStart()
                    .appendLiteral(" Čas: ")
                    .appendPattern("H:mm")
                    .optionalEnd()
                    //default values for time
                    .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
                    .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
                    .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
                    .toFormatter(LOCALE),
            new DateTimeFormatterBuilder()
                    .appendLiteral("Datum: ")
                    .appendPattern("d. M. yyyy")
                    .appendLiteral(" Čas: ")
                    .appendPattern("H.:mm")
                    .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
                    .toFormatter(LOCALE));

    @Override
    public final String getVersion() {
        return VERSION;
    }

    @Override
    protected final ParsedTender preProcessParsedItem(final ParsedTender parsedItem) {
        return parsedItem;
    }

    @Override
    protected final CleanTender postProcessSourceSpecificRules(final ParsedTender parsedTender,
                                                               final CleanTender cleanTender) {
        // related publications can have source form type "preklic/popravek" (or "preklic/popravek NMV" or
        // "preklic/popravek PZP"). We do not know whether it is correction or cancellation and it is cleaned as
        // OTHER, so we set the form type to null
        cleanTender.getPublications()
                .stream()
                .filter(p -> !p.getIsIncluded() && p.getSourceFormType().contains("preklic/popravek"))
                .forEach(p -> p.setFormType(null));

        return cleanTender;
    }

    @SuppressWarnings("unchecked")
    @Override
    protected final void registerSpecificPlugins() {
        pluginRegistry
                .registerPlugin("date", new DatePlugin(DATE_FORMATTERS))
                .registerPlugin("datetime", new DateTimePlugin(DATETIME_FORMATTERS))
                .registerPlugin("bodies", new BodyPlugin(getBuyerTypeMapping(), getBuyerActivitiesMapping()))
                .registerPlugin("lots", new LotPlugin(NUMBER_FORMAT, DATE_FORMATTERS, new HashMap<>()))
                .registerPlugin("publications",
                        new PublicationPlugin(NUMBER_FORMAT, DATE_FORMATTERS, getFormTypeMapping()))
                .registerPlugin("prices", new PricePlugin(NUMBER_FORMAT))
                .registerPlugin("fundings", new FundingsPlugin(NUMBER_FORMAT))
                .registerPlugin("supplyType", new TenderSupplyTypePlugin(getSupplyTypeMapping()))
                .registerPlugin("selectionMethod", new SelectionMethodPlugin(getSelectionMethodMapping()))
                .registerPlugin("procedureType", new TenderProcedureTypePlugin(getProcedureTypeMapping()));
    }

    /**
     * @return buyer activities mapping
     */
    private static Map<Enum, List<String>> getBuyerActivitiesMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(BuyerActivityType.DEFENCE, Arrays.asList("Obramba"));
        mapping.put(BuyerActivityType.GENERAL_PUBLIC_SERVICES, Arrays.asList("Javna uprava"));
        mapping.put(BuyerActivityType.HEALTH, Arrays.asList("Zdravje"));
        mapping.put(BuyerActivityType.POSTAL, Arrays.asList("Poštne storitve"));
        mapping.put(BuyerActivityType.SOCIAL_PROTECTION, Arrays.asList("Socialno varstvo"));
        mapping.put(BuyerActivityType.GAS_AND_HEAT_PRODUCTION, Arrays.asList("Proizvodnja, prenos ali distribucija"
            + " plina in toplote"));
        mapping.put(BuyerActivityType.WATER, Arrays.asList("Voda"));
        mapping.put(BuyerActivityType.ELECTRICITY, Arrays.asList("Električna energija"));
        mapping.put(BuyerActivityType.EDUCATION, Arrays.asList("Izobraževanje"));
        mapping.put(BuyerActivityType.RECREATION_CULTURE_AND_RELIGION, Arrays.asList("Rekreacija, kultura in"
            + " religija"));


               
        return mapping;
    }

    /**
     * @return procedure type mapping
     */
    private static Map<Enum, List<String>> getProcedureTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderProcedureType.OPEN, Arrays.asList("Odprti", "Odprti postopek"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITHOUT_PUBLICATION, Arrays.asList("S pogajanji, brez predhodnega"
            + " javnega razpisa"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITH_PUBLICATION, Arrays.asList("Postopek s pogajanji s predhodnim"
            + " javnim razpisom"));
        
        return mapping;
    }

    /**
     * @return buyer type mapping
     */
    private static Map<Enum, List<String>> getBuyerTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(BuyerType.PUBLIC_BODY, Arrays.asList(
                "Oseba javnega prava"));
        mapping.put(BuyerType.REGIONAL_AUTHORITY, Arrays.asList(
                "Regionalni ali lokalni organ"));
        mapping.put(BuyerType.REGIONAL_AGENCY, Arrays.asList(
                "Regionalna ali lokalna agencija.",
                "Regionalna ali lokalna agencija/urad"));
        mapping.put(BuyerType.NATIONAL_AUTHORITY, Arrays.asList(
                "Ministrstvo ali kateri koli drug nacionalni ali zvezni organ,  vključno z regionalnimi ali " +
                        "lokalnimi pododdelki",
                "Ministrstvo ali kateri koli drugi državni organ,  vključno z njihovimi regionalnimi ali " +
                        "lokalnimi oddelki."));
        mapping.put(BuyerType.NATIONAL_AGENCY, Arrays.asList("Nacionalna ali zvezna agencija/urad"));
        mapping.put(BuyerType.OTHER, Arrays.asList("Druga oseba javnega prava"));


        return mapping;
    }

    /**
     * @return form type mapping
     */
    private Map<Enum, List<String>> getFormTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(PublicationFormType.CONTRACT_NOTICE, Arrays.asList(
                "EU 2 - SL", "EU 5 - SL", "EU 7 - SL", "EU 12 - SL", "EU 17 - SL", "EU 21 - SL",
                "PZP", "PZPPO1 - ZJN-2", "PZPPO1 - ZJNPOV", "PZPPO1 - ZJNVETPS",
                "NMV1"));
        mapping.put(PublicationFormType.CONTRACT_AWARD, Arrays.asList(
                "EU 3 - SL", "EU 6 - SL", "EU 13 - SL", "EU 18 - SL", "EU 25 - SL",
                "PZPPO2 - ZJN-2", "PZPPO2 - ZJNPOV", "PZPPO2 - ZJNVETPS",
                "NMV2", "OS - ZJNVETPS"));
        mapping.put(PublicationFormType.CONTRACT_CANCELLATION, Arrays.asList(
                "preklic/popravek - Nedokončani postopek",
                "preklic/popravek NMV - Nedokončani postopek",
                "preklic/popravek PZP - Nedokončani postopek"));
        mapping.put(PublicationFormType.CONTRACT_UPDATE, Arrays.asList(
                "EU 14 - SL", "EU 20 - SL",
                "popravek OS",
                "preklic/popravek - Popravek",
                "preklic/popravek NMV - Popravek",
                "preklic/popravek PZP - Popravek",
                "preklic/popravek - Dodatne informacije",
                "preklic/popravek NMV - Dodatne informacije",
                "preklic/popravek PZP - Dodatne informacije"));
        mapping.put(PublicationFormType.PRIOR_INFORMATION_NOTICE, Arrays.asList(
                "EU 1 - SL", "EU 4 - SL", "EU 15 - SL", "EU 16 - SL"));
        mapping.put(PublicationFormType.CONTRACT_IMPLEMENTATION, Arrays.asList(
                "OS - ZJN-2", "OS - ZJNPOV"));

        return mapping;
    }

    /**
     * @return tender supply type mapping
     */
    private Map<Enum, List<String>> getSupplyTypeMapping() {
        Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderSupplyType.WORKS, Arrays.asList("Gradnje"));
        mapping.put(TenderSupplyType.SUPPLIES, Arrays.asList("Dobava blaga", "Blago"));
        mapping.put(TenderSupplyType.SERVICES, Arrays.asList("Storitve"));

        return mapping;
    }

    /**
     * @return selection method mapping
     */
    private Map<Enum, List<String>> getSelectionMethodMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(SelectionMethod.MEAT, Arrays.asList(
            "Ekonomsko najugodnejša ponudba glede na:",
            "Ekonomsko najugodnejša ponudba glede na spodaj navedena merila:",
            "Ekonomsko najugodnejša ponudba glede na merila v specifikacijah, povabilu k oddaji ponudbe ali " +
                    "začetku pogajanj ter v opisni dokumentaciji",
            "Ekonomsko najugodnejša ponudba glede na merila v specifikacijah, povabilu k oddaji ponudbe ali " +
                    "začetku pogajanj"));
        mapping.put(SelectionMethod.LOWEST_PRICE, Arrays.asList(
                "Najnizja cena", "Najnižja cena"));

        return mapping;
    }

}
