package eu.digiwhist.worker.hu.clean;

import eu.digiwhist.worker.clean.BaseDigiwhistTenderCleaner;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.BuyerActivityType;
import eu.dl.dataaccess.dto.codetables.BuyerType;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.AddressPlugin;
import eu.dl.worker.clean.plugin.BodyPlugin;
import eu.dl.worker.clean.plugin.DateTimePlugin;
import eu.dl.worker.clean.plugin.LotPlugin;
import eu.dl.worker.clean.plugin.PricePlugin;
import eu.dl.worker.clean.plugin.PublicationPlugin;
import eu.dl.worker.clean.plugin.TenderProcedureTypePlugin;

import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import static java.time.format.DateTimeFormatter.ISO_LOCAL_DATE;

/**
 * Tender cleaner for Hungary old data.
 *
 * @author Marek Mikes
 */
public class HungaryOldDataTenderCleaner extends BaseDigiwhistTenderCleaner {
    private static final String VERSION = "1";

    private static final Locale LOCALE = new Locale("hu");

    private static final NumberFormat NUMBER_FORMAT = NumberFormat.getInstance(LOCALE);

    private static final List<DateTimeFormatter> DATETIME_FORMATTERS = Arrays.asList(
            ISO_LOCAL_DATE, // equals to 'yyyy-MM-dd'
            DateTimeFormatter.ofPattern("yyyy/MM/dd"));

    private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd 00:00:00");

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
        return cleanTender;
    }

    @SuppressWarnings("unchecked")
    @Override
    protected final void registerSpecificPlugins() {
        pluginRegistry
            .registerPlugin("datetime", new DateTimePlugin(DATETIME_FORMATTERS))
            .registerPlugin("bodies", new BodyPlugin(getBuyerTypeMapping(), getBuyerActivityMapping()))
            .registerPlugin("address", new AddressPlugin())
            .registerPlugin("lots", new LotPlugin(NUMBER_FORMAT, Collections.emptyList(), new HashMap<>()))
            .registerPlugin("publications", new PublicationPlugin(NUMBER_FORMAT, DATE_FORMATTER, null))
            .registerPlugin("procedureType", new TenderProcedureTypePlugin(getProcedureTypeMapping(), null))
            .registerPlugin("prices", new PricePlugin(NUMBER_FORMAT));
    }

    /**
     * @return buyer type mapping
     */
    private static Map<Enum, List<String>> getBuyerTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(BuyerType.REGIONAL_AUTHORITY, Arrays.asList("3"));
        mapping.put(BuyerType.REGIONAL_AGENCY, Arrays.asList("9", "10"));
        mapping.put(BuyerType.NATIONAL_AUTHORITY, Arrays.asList("1", "7"));
        mapping.put(BuyerType.NATIONAL_AGENCY, Arrays.asList("8"));
        mapping.put(BuyerType.PUBLIC_BODY, Arrays.asList("5"));

        return mapping;
    }

    /**
     * @return buyer activity mapping
     */
    private Map<Enum, List<String>> getBuyerActivityMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(BuyerActivityType.DEFENCE, Arrays.asList("Védelem", "Védelem false"));
        mapping.put(BuyerActivityType.ECONOMIC_AND_FINANCIAL_AFFAIRS, Arrays.asList("Gazdasági és pénzügyek",
                "Gazdasági és pénzügyek false"));
        mapping.put(BuyerActivityType.EDUCATION, Arrays.asList("Oktatás", "Oktatás false", "Oktatás true",
                "Oktatás  Egészségügy"));
        mapping.put(BuyerActivityType.ELECTRICITY, Arrays.asList("Villamosenergia", "Villamos energia",
                "falseVillamos energia", "false Villamos energia"));
        mapping.put(BuyerActivityType.ENVIRONMENT, Arrays.asList("Környezetvédelem", "Környezetvédelem false",
                "Környezet", "Környezetvédelem true"));
        mapping.put(BuyerActivityType.GAS_AND_HEAT_PRODUCTION, Arrays.asList(
                "Gáz- és hőenergia termelése, szállítása és elosztása",
                "falseGáz- és hőenergia termelése, szállítása és elosztása"));
        mapping.put(BuyerActivityType.GENERAL_PUBLIC_SERVICES, Arrays.asList("Általános közszolgáltatások",
                "Általános közszolgáltatások false", "Általános közszolgáltatások true"));
        mapping.put(BuyerActivityType.HEALTH, Arrays.asList("Egészségügy"));
        mapping.put(BuyerActivityType.POSTAL, Arrays.asList("Postai szolgáltatások"));
        mapping.put(BuyerActivityType.PUBLIC_ORDER_AND_SAFETY, Arrays.asList("Közrend és biztonság",
                "Közrend és biztonság false"));
        mapping.put(BuyerActivityType.RAILWAY, Arrays.asList("Vasúti szolgáltatások", "falseVasúti szolgáltatások"));
        mapping.put(BuyerActivityType.RECREATION_CULTURE_AND_RELIGION, Arrays.asList("Szabadidő, kultúra és vallás",
                "Szabadidő, kultúra és vallás false"));
        mapping.put(BuyerActivityType.SOCIAL_PROTECTION, Arrays.asList("Szociális védelem", "Szociális védelem false"));
        mapping.put(BuyerActivityType.URBAN_TRANSPORT, Arrays.asList(
                "Városi vasúti, villamos-, trolibusz- vagy autóbusz- szolgáltatások",
                "falseVárosi vasúti, villamos-, trolibusz- vagy autóbusz szolgáltatások",
                "Városi vasúti, villamos-, trolibusz- vagy autóbusz szolgáltatások",
                "Városi vasúti, villamos-, trolibusz- és autóbusz szolgáltatások"));
        mapping.put(BuyerActivityType.WATER, Arrays.asList("Víz", "falseVíz", "vízügy"));

        return mapping;
    }

    /**
     * @return procedure type mapping for cleaning process
     */
    private static Map<Enum, List<String>> getProcedureTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderProcedureType.OPEN, Arrays.asList("1", "10", "101", "108", "112", "118", "218", "401", "410",
                "501", "601", "618", "701", "718", "100101", "100112"));
        mapping.put(TenderProcedureType.RESTRICTED, Arrays.asList("2", "3", "11", "102", "103", "402", "403", "502",
                "515", "602", "603", "702", "100102"));
        mapping.put(TenderProcedureType.COMPETITIVE_DIALOG, Arrays.asList("4", "404"));
        mapping.put(TenderProcedureType.NEGOTIATED, Arrays.asList("6", "12", "106", "406", "606", "100105"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITH_PUBLICATION, Arrays.asList("5", "14", "105", "110", "116",
                "205", "405", "505", "516", "517", "605", "705"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITHOUT_PUBLICATION, Arrays.asList("7", "15", "107", "111", "117",
                "207", "407", "507", "607", "707"));
        mapping.put(TenderProcedureType.OTHER, Arrays.asList("0", "9", "13", "20", "100", "221", "222", "408", "409",
                "508", "608", "609", "709", "888", "999"));

        return mapping;
    }

}
