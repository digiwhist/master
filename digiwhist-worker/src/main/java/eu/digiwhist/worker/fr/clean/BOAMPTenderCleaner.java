package eu.digiwhist.worker.fr.clean;

import eu.digiwhist.worker.clean.BaseDigiwhistTenderCleaner;
import eu.digiwhist.worker.fr.BOAMPTenderUtils;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.BuyerActivityType;
import eu.dl.dataaccess.dto.codetables.BuyerType;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.codetables.TenderSupplyType;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.AddressPlugin;
import eu.dl.worker.clean.plugin.AwardCriteriaPlugin;
import eu.dl.worker.clean.plugin.BodyPlugin;
import eu.dl.worker.clean.plugin.CorrigendumPlugin;
import eu.dl.worker.clean.plugin.DatePlugin;
import eu.dl.worker.clean.plugin.DateTimePlugin;
import eu.dl.worker.clean.plugin.LotPlugin;
import eu.dl.worker.clean.plugin.PricePlugin;
import eu.dl.worker.clean.plugin.PublicationPlugin;
import eu.dl.worker.clean.plugin.TenderProcedureTypePlugin;
import eu.dl.worker.clean.plugin.TenderSupplyTypePlugin;

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.format.SignStyle;
import java.time.temporal.ChronoField;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import static java.time.format.DateTimeFormatter.ISO_LOCAL_DATE;

/**
 * Tender cleaner for France.
 *
 * @author Marek Mikes
 */
public class BOAMPTenderCleaner extends BaseDigiwhistTenderCleaner {
    private static final String VERSION = "1";

    private static final Locale LOCALE = new Locale("fr");

    private static final List<NumberFormat> NUMBER_FORMATS = new ArrayList<>();
    static {
        NUMBER_FORMATS.add(NumberFormat.getInstance(LOCALE));

        // we want parse numbers like "137 640", too
        DecimalFormatSymbols formatSymbols = new DecimalFormatSymbols(LOCALE);
        formatSymbols.setDecimalSeparator(','); // I do not know the separator, I found only natural number yet.
        formatSymbols.setGroupingSeparator(' ');
        NUMBER_FORMATS.add(new DecimalFormat("#,##0.###", formatSymbols));
    }

    private static final List<DateTimeFormatter> DATE_FORMATTERS = Arrays.asList(
            DateTimeFormatter.ofPattern("d MMMM yyyy", LOCALE),
            new DateTimeFormatterBuilder()
                    .appendLiteral("1er ")
                    .appendPattern("MMMM yyyy")
                    .parseDefaulting(ChronoField.DAY_OF_MONTH, 1)
                    .toFormatter(LOCALE),
            DateTimeFormatter.ofPattern("dd/MM/yyyy", LOCALE),
            ISO_LOCAL_DATE); // equals to 'yyyy-MM-dd'

    private static final List<DateTimeFormatter> DATETIME_FORMATTERS = Arrays.asList(
            new DateTimeFormatterBuilder()
                    .appendPattern("d MMMM yyyy")
                    //optional time
                    .optionalStart()
                    .appendLiteral(" , à ")
                    .appendValue(ChronoField.HOUR_OF_DAY, 1, 2, SignStyle.NEVER)
                    .appendLiteral(" heures")
                    .optionalEnd()
                    //default values for time
                    .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
                    .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
                    .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
                    .toFormatter(LOCALE),
            new DateTimeFormatterBuilder()
                    .appendPattern("d MMMM yyyy")
                    //optional time
                    .optionalStart()
                    .appendLiteral(" , à ")
                    .appendValue(ChronoField.HOUR_OF_DAY, 1, 2, SignStyle.NEVER)
                    .appendLiteral(" h ")
                    .appendValue(ChronoField.MINUTE_OF_HOUR, 1, 2, SignStyle.NEVER)
                    .optionalEnd()
                    //default values for time
                    .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
                    .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
                    .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
                    .toFormatter(LOCALE),
            new DateTimeFormatterBuilder()
                    .appendPattern("d MMMM yyyy")
                    //optional time
                    .optionalStart()
                    .appendLiteral(" ")
                    .appendValue(ChronoField.HOUR_OF_DAY, 1, 2, SignStyle.NEVER)
                    .appendLiteral(" heures")
                    .optionalEnd()
                    //default values for time
                    .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
                    .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
                    .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
                    .toFormatter(LOCALE),
            new DateTimeFormatterBuilder()
                    .appendPattern("d MMMM yyyy")
                    //optional time
                    .optionalStart()
                    .appendLiteral(" ")
                    .appendValue(ChronoField.HOUR_OF_DAY, 1, 2, SignStyle.NEVER)
                    .appendLiteral(" h ")
                    .appendValue(ChronoField.MINUTE_OF_HOUR, 1, 2, SignStyle.NEVER)
                    .optionalEnd()
                    //default values for time
                    .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
                    .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
                    .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
                    .toFormatter(LOCALE),
            new DateTimeFormatterBuilder()
                    .appendPattern("d MMMM yyyy")
                    //optional time
                    .optionalStart()
                    .appendLiteral(" , avant ")
                    .appendValue(ChronoField.HOUR_OF_DAY, 1, 2, SignStyle.NEVER)
                    .appendLiteral(" heures")
                    .optionalEnd()
                    //default values for time
                    .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
                    .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
                    .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
                    .toFormatter(LOCALE),
            new DateTimeFormatterBuilder()
                    .appendPattern("d MMMM yyyy")
                    //optional time
                    .optionalStart()
                    .appendLiteral(" , avant ")
                    .appendValue(ChronoField.HOUR_OF_DAY, 1, 2, SignStyle.NEVER)
                    .appendLiteral(" h ")
                    .appendValue(ChronoField.MINUTE_OF_HOUR, 1, 2, SignStyle.NEVER)
                    .optionalEnd()
                    //default values for time
                    .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
                    .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
                    .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
                    .toFormatter(LOCALE),
            new DateTimeFormatterBuilder()
                    .appendLiteral("1er ")
                    .appendPattern("MMMM yyyy")
                    .parseDefaulting(ChronoField.DAY_OF_MONTH, 1)
                    //optional time
                    .optionalStart()
                    .appendLiteral(" , à ")
                    .appendValue(ChronoField.HOUR_OF_DAY, 1, 2, SignStyle.NEVER)
                    .appendLiteral(" heures")
                    .optionalEnd()
                    //default values for time
                    .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
                    .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
                    .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
                    .toFormatter(LOCALE),
            new DateTimeFormatterBuilder()
                    .appendLiteral("1er ")
                    .appendPattern("MMMM yyyy")
                    .parseDefaulting(ChronoField.DAY_OF_MONTH, 1)
                    //optional time
                    .optionalStart()
                    .appendLiteral(" , à ")
                    .appendValue(ChronoField.HOUR_OF_DAY, 1, 2, SignStyle.NEVER)
                    .appendLiteral(" h ")
                    .appendValue(ChronoField.MINUTE_OF_HOUR, 1, 2, SignStyle.NEVER)
                    .optionalEnd()
                    //default values for time
                    .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
                    .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
                    .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
                    .toFormatter(LOCALE),
            new DateTimeFormatterBuilder()
                    .appendLiteral("1er ")
                    .appendPattern("MMMM yyyy")
                    .parseDefaulting(ChronoField.DAY_OF_MONTH, 1)
                    //optional time
                    .optionalStart()
                    .appendLiteral(" ")
                    .appendValue(ChronoField.HOUR_OF_DAY, 1, 2, SignStyle.NEVER)
                    .appendLiteral(" heures")
                    .optionalEnd()
                    //default values for time
                    .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
                    .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
                    .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
                    .toFormatter(LOCALE),
            new DateTimeFormatterBuilder()
                    .appendLiteral("1er ")
                    .appendPattern("MMMM yyyy")
                    .parseDefaulting(ChronoField.DAY_OF_MONTH, 1)
                    //optional time
                    .optionalStart()
                    .appendLiteral(" ")
                    .appendValue(ChronoField.HOUR_OF_DAY, 1, 2, SignStyle.NEVER)
                    .appendLiteral(" h ")
                    .appendValue(ChronoField.MINUTE_OF_HOUR, 1, 2, SignStyle.NEVER)
                    .optionalEnd()
                    //default values for time
                    .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
                    .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
                    .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
                    .toFormatter(LOCALE),
            new DateTimeFormatterBuilder()
                    .appendPattern("yyyy-MM-dd")
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
                .registerPlugin("date", new DatePlugin(DATE_FORMATTERS))
                .registerPlugin("datetime", new DateTimePlugin(DATETIME_FORMATTERS))
                .registerPlugin("bodies", new BodyPlugin(getBuyerTypeMapping(), getBuyerActivityMapping()))
                .registerPlugin("address", new AddressPlugin())
                .registerPlugin("lots", new LotPlugin(NUMBER_FORMATS, DATE_FORMATTERS, new HashMap<>()))
                .registerPlugin("awardCriteria", new AwardCriteriaPlugin(NUMBER_FORMATS))
                .registerPlugin("publications",
                        new PublicationPlugin(NUMBER_FORMATS, DATE_FORMATTERS, BOAMPTenderUtils.FORM_TYPE_MAPPING))
                .registerPlugin("procedureType", new TenderProcedureTypePlugin(getProcedureTypeMapping(), null))
                .registerPlugin("prices", new PricePlugin(NUMBER_FORMATS))
                .registerPlugin("corrections", new CorrigendumPlugin(NUMBER_FORMATS, DATE_FORMATTERS));
    }

    /**
     * @return supply type mapping for cleaning process
     */
    private static Map<Enum, List<String>> getSupplyTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        // todo: implement when task #3362 is done
        mapping.put(TenderSupplyType.WORKS, Arrays.asList("Travaux de construction"));

        return mapping;
    }

    /**
     * @return buyer activities mapping
     */
    private Map<Enum, List<String>> getBuyerActivityMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(BuyerActivityType.GENERAL_PUBLIC_SERVICES, Arrays.asList("act_serv_adm_pub"));
        mapping.put(BuyerActivityType.HEALTH, Arrays.asList("sante"));
        mapping.put(BuyerActivityType.ENVIRONMENT, Arrays.asList("environnement"));
        mapping.put(BuyerActivityType.DEFENCE, Arrays.asList("def"));
        mapping.put(BuyerActivityType.EDUCATION, Arrays.asList("education"));
        mapping.put(BuyerActivityType.WATER, Arrays.asList("eau"));
        mapping.put(BuyerActivityType.SOCIAL_PROTECTION, Arrays.asList("protection_sociale"));
        mapping.put(BuyerActivityType.RECREATION_CULTURE_AND_RELIGION, Arrays.asList("lois_cult_rel"));
        mapping.put(BuyerActivityType.ECONOMIC_AND_FINANCIAL_AFFAIRS, Arrays.asList("aff_eco_fin"));
        mapping.put(BuyerActivityType.PUBLIC_ORDER_AND_SAFETY, Arrays.asList("ordre_sec_pub"));
        mapping.put(BuyerActivityType.URBAN_TRANSPORT, Arrays.asList("ser_chemin_fer_tram_bus"));
        mapping.put(BuyerActivityType.AIRPORT, Arrays.asList("act_aeroport"));
        mapping.put(BuyerActivityType.PORT, Arrays.asList("act_port"));
        mapping.put(BuyerActivityType.ELECTRICITY, Arrays.asList("electricite"));
        mapping.put(BuyerActivityType.POSTAL, Arrays.asList("ser_post"));
        mapping.put(BuyerActivityType.GAS_AND_OIL_EXTRACTION, Arrays.asList("prosp_extract_petrole_gaz"));
        // I have to map the OTHER, otherwise "autre" is mapped on HEALTH because of fuzzy match (Levenshtein distance)
        mapping.put(BuyerActivityType.OTHER, Arrays.asList("autre"));

        return mapping;
    }

    /**
     * @return procedure type mapping for cleaning process
     */
    private static Map<Enum, List<String>> getProcedureTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderProcedureType.OPEN, Arrays.asList(
                "ouvert"));
        mapping.put(TenderProcedureType.RESTRICTED, Arrays.asList(
                "restreint"));
        mapping.put(TenderProcedureType.NEGOTIATED, Arrays.asList(
                "autre_negocie",
                "negocie_apres_infru"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITH_PUBLICATION, Arrays.asList(
                "marche_negocie"));
        mapping.put(TenderProcedureType.COMPETITIVE_DIALOG, Arrays.asList(
                "dialogue_compe",
                "dialogue_competitif"));
        mapping.put(TenderProcedureType.DESIGN_CONTEST, Arrays.asList(
                "concours_ouvert",
                "concours_restreint"));
        mapping.put(TenderProcedureType.OUTRIGHT_AWARD, Arrays.asList(
                "attribue_sans_pub_joue"));
        mapping.put(TenderProcedureType.OTHER, Arrays.asList(
                "autre",
                "autre_proc",
                "autre_procedure",
                "mo_apres_ouvert",
                "mo_apres_restreint",
                "mo_avec_concours",
                "mo_sans_concours",
                "negociee",
                "negocie",
                "partenariat_innovation",
                "performances",
                "simplifiee",
                "procedure_adaptee",
                "procedure_adapte"));

        return mapping;
    }

    /**
     * @return buyer type mapping
     */
    private static Map<Enum, List<String>> getBuyerTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(BuyerType.PUBLIC_BODY, Arrays.asList(
                "org_droit_public",
                "epn"));
        mapping.put(BuyerType.NATIONAL_AGENCY, Arrays.asList(
                "agence_national"));
        mapping.put(BuyerType.NATIONAL_AUTHORITY, Arrays.asList(
                "etat",
                "min_autorite_nationale"));
        mapping.put(BuyerType.REGIONAL_AGENCY, Arrays.asList(
                "ept",
                "agence_regional"));
        mapping.put(BuyerType.REGIONAL_AUTHORITY, Arrays.asList(
                "commune",
                "autotrite_regional",
                "region"));
        mapping.put(BuyerType.EUROPEAN_AGENCY, Arrays.asList(
                "institution_europeenne"));
        mapping.put(BuyerType.OTHER, Arrays.asList(
                "autre",
                "departement"));

        return mapping;
    }

}
