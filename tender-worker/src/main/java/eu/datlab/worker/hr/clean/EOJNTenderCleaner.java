package eu.datlab.worker.hr.clean;

import eu.datlab.worker.clean.BaseDatlabTenderCleaner;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.BuyerActivityType;
import eu.dl.dataaccess.dto.codetables.BuyerType;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.SelectionMethod;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.codetables.TenderSupplyType;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.AddressPlugin;
import eu.dl.worker.clean.plugin.BodyPlugin;
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
 * Tender parser for Croatia.
 *
 * @author Marek Mikes
 */
public class EOJNTenderCleaner extends BaseDatlabTenderCleaner {
    private static final String VERSION = "1";

    private static final Locale LOCALE = new Locale("hr");

    private static final NumberFormat NUMBER_FORMAT = NumberFormat.getInstance(LOCALE);

    // Date examples:
    // 05.07.2013
    // 27.2.2015
    private static final List<DateTimeFormatter> DATE_FORMATTERS = Arrays.asList(
            DateTimeFormatter.ofPattern("dd.MM.uuuu", LOCALE),
            DateTimeFormatter.ofPattern("d.M.uuuu", LOCALE));

    // Date time examples:
    // 31.12.2012,9:35
    // 31.12.2013,09:00:00
    private static final List<DateTimeFormatter> DATETIME_FORMATTERS = Arrays.asList(
            new DateTimeFormatterBuilder()
                    .appendPattern("dd.MM.uuuu,H:mm")
                    //default values for time
                    .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
                    .toFormatter(LOCALE),
            new DateTimeFormatterBuilder()
                    .appendPattern("dd.MM.uuuu,HH:mm:ss")
                    .toFormatter(LOCALE));

    @Override
    public final String getVersion() {
        return VERSION;
    }

    @Override
    protected final ParsedTender preProcessParsedItem(final ParsedTender parsedItem) {
        // Some publication source IDs are incorrect on source, so we remove it here.
        // We have to remove the IDs which are filled in more than one publication especially to avoid match them
        final ParsedPublication includedPublication = parsedItem.getPublications().get(0);
        assert includedPublication.getIsIncluded().equals(Boolean.TRUE.toString());
        if (includedPublication.getSourceId() != null) {
            switch (includedPublication.getSourceId()) {
                // https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=850157
                case "{OznakaObj}":
                    // https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=453798
                case "Prethodna obavijest o namjeri sklapanja ugovora":
                    // https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=778729
                case "POziv na nadmetanje":
                    // https://eojn.nn.hr/SPIN/APPLICATION/IPN/DocumentManagement/DokumentPodaciFrm.aspx?id=301738
                case "PRETHODNA NAMJERA SKLAPANJA UGOVORA":
                    logger.info("Invalid publication source ID \"{}\" will be removed.",
                            includedPublication.getSourceId());
                    includedPublication.setSourceId(null);
                    break;
                default:
                    break;
            }
        }

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
                .registerPlugin("supplyType", new TenderSupplyTypePlugin(getSupplyTypeMapping()))
                .registerPlugin("datetime", new DateTimePlugin(DATETIME_FORMATTERS))
                .registerPlugin("bodies", new BodyPlugin(bodyTypeMapping(), getBuyerActivityMapping()))
                .registerPlugin("address", new AddressPlugin())
                .registerPlugin("lots", new LotPlugin(NUMBER_FORMAT, DATE_FORMATTERS, new HashMap<>()))
                .registerPlugin("publications",
                        new PublicationPlugin(NUMBER_FORMAT, DATE_FORMATTERS, getFormTypeMapping()))
                .registerPlugin("procedureType", new TenderProcedureTypePlugin(getProcedureTypeMapping()))
                .registerPlugin("prices", new PricePlugin(NUMBER_FORMAT))
                .registerPlugin("selectionMethod", new SelectionMethodPlugin(getSelectionMethodMapping()))
                .registerPlugin("fundings", new FundingsPlugin(NUMBER_FORMAT));
    }

    /**
     * @return supply type mapping for cleaning process
     */
    private static Map<Enum, List<String>> getSupplyTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderSupplyType.SUPPLIES, Arrays.asList("Roba"));
        mapping.put(TenderSupplyType.SERVICES, Arrays.asList("Usluge"));
        mapping.put(TenderSupplyType.WORKS, Arrays.asList("Radovi"));

        return mapping;
    }

    /**
     * @return buyer activities mapping
     */
    private Map<Enum, List<String>> getBuyerActivityMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(BuyerActivityType.AIRPORT, Arrays.asList(
                "Djelatnosti povezane sa zračnim lukama"));
        mapping.put(BuyerActivityType.ECONOMIC_AND_FINANCIAL_AFFAIRS, Arrays.asList(
                "Gospodarstvo i financije"));
        mapping.put(BuyerActivityType.EDUCATION, Arrays.asList(
                "Obrazovanje"));
        mapping.put(BuyerActivityType.ENVIRONMENT, Arrays.asList(
                "Okoliš"));
        mapping.put(BuyerActivityType.ELECTRICITY, Arrays.asList(
                "Električna energija"));
        mapping.put(BuyerActivityType.DEFENCE, Arrays.asList(
                "Obrana"));
        mapping.put(BuyerActivityType.GAS_AND_HEAT_PRODUCTION, Arrays.asList(
                "Proizvodnja, prijenos i distribucija plina i  toplinske energije"));
        mapping.put(BuyerActivityType.GAS_AND_OIL_EXTRACTION, Arrays.asList(
                "Istraživanje i vađenje plina i nafte"));
        mapping.put(BuyerActivityType.GENERAL_PUBLIC_SERVICES, Arrays.asList(
                "Opće javne usluge"));
        mapping.put(BuyerActivityType.HEALTH, Arrays.asList(
                "Zdravstvo"));
        mapping.put(BuyerActivityType.HOUSING_AND_COMMUNITY_AMENITIES, Arrays.asList(
                "Stambeno i komunalno gospodarstvo i usluge"));
        mapping.put(BuyerActivityType.PORT, Arrays.asList(
                "Djelatnosti povezane s lukama"));
        mapping.put(BuyerActivityType.POSTAL, Arrays.asList(
                "Poštanske usluge"));
        mapping.put(BuyerActivityType.PUBLIC_ORDER_AND_SAFETY, Arrays.asList(
                "Javni red i sigurnost",
                "Javni  red i sigurnost"));
        mapping.put(BuyerActivityType.RAILWAY, Arrays.asList(
                "Željezničke usluge"));
        mapping.put(BuyerActivityType.RECREATION_CULTURE_AND_RELIGION, Arrays.asList(
                "Rekreacija, kultura i religija"));
        mapping.put(BuyerActivityType.SOCIAL_PROTECTION, Arrays.asList(
                "Socijalna skrb"));
        mapping.put(BuyerActivityType.URBAN_TRANSPORT, Arrays.asList(
                "Usluge gradske željeznice, tramvaja, trolejbusa ili autobusa"));
        mapping.put(BuyerActivityType.WATER, Arrays.asList(
                "Vodoopskrba",
                "Ostalo: Vodno gospodarstvo"));
        mapping.put(BuyerActivityType.OTHER, Arrays.asList(
                "Ostalo:"));

        return mapping;
    }

    /**
     * @return form type mapping
     */
    private Map<Enum, List<String>> getFormTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(PublicationFormType.CONTRACT_NOTICE, Arrays.asList(
                "Poziv na nadmetanje",
                "Poziv na nadmetanje – sektor",
                "POZIV NA NADMETANJE - SEKTOR",
                "Poziv na nadmetanje za ugovore u području obrane i sigurnosti",
                "Obavijest o nadmetanju",
                "Obavijest o nadmetanju – sektorska nabava",
                "POZIV NA NATJEČAJ"));
        mapping.put(PublicationFormType.CONTRACT_AWARD, Arrays.asList(
                "Obavijest o sklopljenim ugovorima",
                "Obavijest o sklopljenim ugovorima - sektor",
                "OBAVIJEST O SKLOPLJENIM UGOVORIMA - SEKTORSKI",
                "Obavijest o sklopljenim ugovorima u području obrane i sigurnosti",
                "Obavijest o dodjeli ugovora",
                "Obavijest o dodjeli ugovora – sektorska nabava",
                "OBAVIJEST O REZULTATIMA NATJEČAJA",
                "Prethodna obavijest o namjeri sklapanja ugovora"));
        mapping.put(PublicationFormType.CONTRACT_CANCELLATION, Arrays.asList(
                "OBJAVA O PONIŠTENJU NADMETANJA/ ISPRAVKU OBJAVE"));
        mapping.put(PublicationFormType.CONTRACT_UPDATE, Arrays.asList(
                "Ispravak Obavijest o izmjenama ili dodatnim informacijama"));

        return mapping;
    }

    /**
     * @return procedure type mapping for cleaning process
     */
    private static Map<Enum, List<String>> getProcedureTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderProcedureType.COMPETITIVE_DIALOG, Arrays.asList(
                "Natjecateljski dijalog"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITH_PUBLICATION, Arrays.asList(
                "Pregovarački s prethodnom objavom",
                "Pregovarački s prethodnom objavom zbog žurnosti"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITHOUT_PUBLICATION, Arrays.asList(
                "Pregovarački bez prethodne objave",
                "Sklapanje ugovora bez prethodne objave poziva na nadmetanje (u slučajevima navedenim u Odjeljku 2 " +
                        "Priloga D1) Obrazloženje odluke za sklapanje ugovora bez objave poziva na nadmetanje: " +
                        "molimo popunite Prilog D1"));
        mapping.put(TenderProcedureType.OPEN, Arrays.asList(
                "Otvoreni"));
        mapping.put(TenderProcedureType.RESTRICTED, Arrays.asList(
                "Ograničeni",
                "Ograničeni zbog žurnosti"));

        return mapping;
    }

    /**
     * @return selection method mapping for cleaning process
     */
    private static Map<Enum, List<String>> getSelectionMethodMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(SelectionMethod.LOWEST_PRICE, Arrays.asList(
                "Najniža cijena", "NajniĹľa cijena"));
        mapping.put(SelectionMethod.MEAT, Arrays.asList(
                "Ekonomski najpovoljnija ponudaÂ", "Ekonomski najpovoljnija ponuda u odnosu na", "Ekonomski najpovoljnija ponuda"));

        return mapping;
    }

    /**
     * @return body type mapping
     */
    private static Map<Enum, List<String>> bodyTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(BuyerType.REGIONAL_AUTHORITY,
                Arrays.asList("Pravne osobe koje su osnovane za određene svrhe radi zadovoljavanja potreba u općem interesu"));

        return mapping;
    }
}
