package eu.datlab.worker.sk.clean;

import eu.datlab.worker.clean.BaseDatlabTenderCleaner;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.clean.CleanTenderLot;
import eu.dl.dataaccess.dto.codetables.BuyerActivityType;
import eu.dl.dataaccess.dto.codetables.BuyerType;
import eu.dl.dataaccess.dto.codetables.CountryCode;
import eu.dl.dataaccess.dto.codetables.NpwpReason;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.SelectionMethod;
import eu.dl.dataaccess.dto.codetables.TenderLotStatus;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.codetables.TenderSupplyType;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.parsed.BaseParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedPayment;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.dataaccess.dto.parsed.ParsedUnitPrice;
import eu.dl.worker.clean.plugin.AddressPlugin;
import eu.dl.worker.clean.plugin.AwardCriteriaPlugin;
import eu.dl.worker.clean.plugin.BodyPlugin;
import eu.dl.worker.clean.plugin.CorrigendumPlugin;
import eu.dl.worker.clean.plugin.DocumentPlugin;
import eu.dl.worker.clean.plugin.FundingsPlugin;
import eu.dl.worker.clean.plugin.IntegerPlugin;
import eu.dl.worker.clean.plugin.NpwpReasonPlugin;
import eu.dl.worker.clean.plugin.SelectionMethodPlugin;
import eu.dl.worker.clean.plugin.TenderSupplyTypePlugin;
import eu.dl.worker.clean.utils.CodeTableUtils;

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
import java.util.function.Predicate;

/**
 * Cleaner for the SK UVO source.
 *
 * @author Michal Riha
 */
public class UvoTenderCleaner extends BaseDatlabTenderCleaner {
    private static final String VERSION = "1.1";

    private static final Locale LOCALE = new Locale("sk");

    private static final NumberFormat NUMBER_FORMAT;
    static {
        DecimalFormatSymbols formatSymbols = new DecimalFormatSymbols(LOCALE);
        formatSymbols.setDecimalSeparator(',');
        formatSymbols.setGroupingSeparator(' ');
        NUMBER_FORMAT = new DecimalFormat("#,##0.###", formatSymbols);
    }

    private static final List<DateTimeFormatter> DATE_FORMATTERS = Arrays.asList(
        DateTimeFormatter.ofPattern("d.[ ]M.[ ]uuuu"),
        DateTimeFormatter.ofPattern("d. M. uuuu."),
        DateTimeFormatter.ofPattern("dd.MM.uuuu"),
        DateTimeFormatter.ofPattern("dd.MM.uuuu."),
        DateTimeFormatter.ofPattern("dd.MM.uuuu "),
        DateTimeFormatter.ofPattern("[ ]d.M.uuuu HH:mm"),
        DateTimeFormatter.ofPattern("uuuu-M-d")
    );

    /**
     * This DateTimeFormatter parses following datetime strings.
     * - d. M. yyyy (time is omitted)
     * - d. M. yyyy HH:mm
     * - d. M. yyyy. (time is omitted)
     * - d. M. yyyy. HH:mm
     * - dd.MM.yyyy (time is omitted)
     * - dd.MM.yyyy HH:mm
     * - dd.MM.yyyy. (time is omitted)
     * - dd.MM.yyyy. HH:mm
     */
    private static final List<DateTimeFormatter> DATETIME_FORMATTER = Arrays.asList(
            new DateTimeFormatterBuilder().appendPattern("d. M. yyyy")
                    //optional time
                    .optionalStart()
                    .appendLiteral(" ")
                    .appendValue(ChronoField.HOUR_OF_DAY, 1, 2, SignStyle.NEVER)
                    .appendLiteral(".")
                    .appendValue(ChronoField.MINUTE_OF_HOUR, 1, 2, SignStyle.NEVER)
                    .optionalEnd()
                    //default values for time
                    .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
                    .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
                    .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
                    .toFormatter(),
            new DateTimeFormatterBuilder().appendPattern("d. M. yyyy.")
                    //optional time
                    .optionalStart()
                    .appendLiteral(" ")
                    .appendValue(ChronoField.HOUR_OF_DAY, 1, 2, SignStyle.NEVER)
                    .appendLiteral(".")
                    .appendValue(ChronoField.MINUTE_OF_HOUR, 1, 2, SignStyle.NEVER)
                    .optionalEnd()
                    //default values for time
                    .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
                    .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
                    .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
                    .toFormatter(),
            new DateTimeFormatterBuilder().appendPattern("d. M. yyyy.")
                    //optional time
                    .optionalStart()
                    .appendLiteral(" ")
                    .appendValue(ChronoField.HOUR_OF_DAY, 1, 2, SignStyle.NEVER)
                    .appendLiteral(".")
                    .appendLiteral(" ")
                    .appendValue(ChronoField.MINUTE_OF_HOUR, 1, 2, SignStyle.NEVER)
                    .optionalEnd()
                    //default values for time
                    .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
                    .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
                    .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
                    .toFormatter(),
            new DateTimeFormatterBuilder().appendPattern("d. M. yyyy.")
                    //optional time
                    .optionalStart()
                    .appendLiteral(" ")
                    .appendValue(ChronoField.HOUR_OF_DAY, 1, 2, SignStyle.NEVER)
                    .appendValue(ChronoField.MINUTE_OF_HOUR, 1, 2, SignStyle.NEVER)
                    .optionalEnd()
                    //default values for time
                    .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
                    .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
                    .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
                    .toFormatter(),
            new DateTimeFormatterBuilder().appendPattern("[ ]d.M.yyyy")
                    //optional time
                    .optionalStart()
                    .appendLiteral(" ")
                    .appendValue(ChronoField.HOUR_OF_DAY, 1, 2, SignStyle.NEVER)
                    .appendLiteral(":")
                    .appendValue(ChronoField.MINUTE_OF_HOUR, 1, 2, SignStyle.NEVER)
                    .optionalEnd()
                    //default values for time
                    .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
                    .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
                    .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
                    .toFormatter(),
            new DateTimeFormatterBuilder().appendPattern("dd.MM.yyyy.")
                    //optional time
                    .optionalStart()
                    .appendLiteral(" ")
                    .appendValue(ChronoField.HOUR_OF_DAY, 1, 2, SignStyle.NEVER)
                    .appendLiteral(":")
                    .appendValue(ChronoField.MINUTE_OF_HOUR, 1, 2, SignStyle.NEVER)
                    .optionalEnd()
                    //default values for time
                    .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
                    .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
                    .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
                    .toFormatter(),
            DateTimeFormatter.ofPattern("[ ]d.M.uuuu HH:mm"));

    @SuppressWarnings("unchecked")
    @Override
    protected final void registerSpecificPlugins() {
        Map<String, Map<Enum, List<String>>> lotMappings = new HashMap<>();
        lotMappings.put("countryMapping", countryMapping());
        lotMappings.put("statusMapping", lotStatusMapping());

        pluginRegistry
            .registerPlugin("integerPlugin", new IntegerPlugin(NUMBER_FORMAT))
            .registerPlugin("supplyType", new TenderSupplyTypePlugin(supplyTypeMapping()))
            .registerPlugin("procedureType", new UvoTenderProcedureTypePlugin(procedureTypeMapping(), Arrays.asList("Užšia súťaž so" +
                " skrátenými lehotami", "Urychlené vyjednávací", "Urychlené omezené", "Urychlené jednací", "Urychlené užší",
                "Užšia súťaž so zrýchleným postupom", "Rokovacie konanie zrýchleným postupom", "Rokovacie konanie so zverejnením so" +
                " skrátenými lehotami")))
            .registerPlugin("date", new UvoTenderDatePlugin(DATETIME_FORMATTER))
            .registerPlugin("datetime", new UvoTenderDateTimePlugin(DATETIME_FORMATTER))
            .registerPlugin("bodies", new BodyPlugin(bodyTypeMapping(), bodyActivityMapping(), countryMapping()))
            .registerPlugin("lots", new UvoTenderLotPlugin(NUMBER_FORMAT, DATE_FORMATTERS, lotMappings))
            .registerPlugin("documents", new DocumentPlugin(NUMBER_FORMAT, DATE_FORMATTERS, null))
            .registerPlugin("prices", new UvoTenderPricePlugin(NUMBER_FORMAT))
            .registerPlugin("fundings", new FundingsPlugin(NUMBER_FORMAT))
            .registerPlugin("corrections", new CorrigendumPlugin(NUMBER_FORMAT, DATETIME_FORMATTER))
            .registerPlugin("address", new AddressPlugin())
            .registerPlugin("selectionMethod", new SelectionMethodPlugin(selectionMethodMapping()))
            .registerPlugin("awardCriteria", new AwardCriteriaPlugin(NUMBER_FORMAT))
            .registerPlugin("publications", new UvoTenderPublicationPlugin(NUMBER_FORMAT, DATE_FORMATTERS, formTypeMapping()))
            .registerPlugin("npwpReason", new NpwpReasonPlugin(npwpMapping()));
    }

    /**
     * @return lot status mapping
     */
    private Map<Enum, List<String>> lotStatusMapping() {
        return CodeTableUtils.enumToMapping(TenderLotStatus.class);
    }

    /**
     * @return selection Method mapping for cleaning process
     */
    private Map<Enum, List<String>> npwpMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(NpwpReason.RESEARCH_PROCUREMENT, Arrays.asList("b)"));
        mapping.put(NpwpReason.AUTHORSHIP_RIGHTS_EXCLUSIVITY, Arrays.asList("c3)"));
        mapping.put(NpwpReason.ADVANTAGEOUS_CONDITIONS, Arrays.asList("i)"));
        mapping.put(NpwpReason.TECHNICAL_EXCLUSIVITY, Arrays.asList("c1)"));
        mapping.put(NpwpReason.NO_VALID_OFFERS_IN_PRECEEDING_PROCUREMENT, Arrays.asList("a)", "a1)", "a2)"));
        mapping.put(NpwpReason.EMERGENCY, Arrays.asList("d)"));
        mapping.put(NpwpReason.COMMODITY_MARKET, Arrays.asList("h)"));
        mapping.put(NpwpReason.ADDITIONAL_WORK, Arrays.asList("e)", "f)", "e2)"));
        mapping.put(NpwpReason.PROPOSAL_CONTEST_FOLLOW_UP, Arrays.asList("g)"));
        mapping.put(NpwpReason.ART_EXCLUSIVITY, Arrays.asList("c2)"));

        return mapping;
    }

    /**
     * @return selection Method mapping for cleaning process
     */
    private Map<Enum, List<String>> selectionMethodMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(SelectionMethod.MEAT, Arrays.asList(
                "Ekonomicky najvýhodnejšia ponuka z hľadiska", "Ekonomicky najvýhodnejšia ponuka .",
                "Ekonomicky najvýhodnejšia ponuka z hľadiska kritérií uvedených ďalej (kritériá na vyhodnotenie ponúk" +
                        " je potrebné uviesť s ich relatívnou váhou alebo v zostupnom poradí dôležitosti, ak nemožno " +
                        "z preukázateľných dôvodov uviesť ich relatívnu váhu). Kritériá sú stanovené v súťažných podk" +
                        "ladoch v kritériách na vyhodnotenie ponúk s ich relatívnou váhou", "Nižšie uvedené kritéria",
                "Kritériá kvality", "Cena nie je jediným kritériom výberu a všetky kritériá sú uvedené len v súťažných podkladoch"
                ));
        mapping.put(SelectionMethod.LOWEST_PRICE, Arrays.asList("Najnižšia cena.", "Najnižšia cena",
                "Najnižšia cena za celú koncesiu.", "Cena", "Kritéria kvality: Nie",
                "Elektronická aukcia bude realizovaná certifikovaným systémom na uskutočnenie elektronickej aukcie EV" +
                        "OB portáli www.verejneaukcie.sk, kritériom je najnižšia cena za predmet obstarávania. Podrob" +
                        "né informácie sú uvedené v súťažných podkladoch. Elektronická aukcia sa uskutoční v súlade s" +
                        " § 54 ZVO, podrobné informácie sú uvedené v Súťažných podkladoch."));
        return mapping;
    }

    @Override
    public final String getVersion() {
        return VERSION;
    }

    @Override
    protected final CleanTender postProcessSourceSpecificRules(final ParsedTender parsedTender, final CleanTender cleanTender) {
        // On contract cancellations with no cancellation date, set cancellation date as publication date
        if (cleanTender.getPublications() != null && cleanTender.getPublications().get(0).getFormType() != null
                && cleanTender.getPublications().get(0).getFormType().equals(PublicationFormType.CONTRACT_CANCELLATION)
                && cleanTender.getCancellationDate() == null) {
            cleanTender.setCancellationDate(cleanTender.getPublications().get(0).getPublicationDate());
        }

        // validBidsCount is invalidBidsCount, recalculating here
        if (cleanTender.getLots() != null) {
            for (CleanTenderLot lot : cleanTender.getLots()) {
                if (lot.getBidsCount() != null && lot.getValidBidsCount() != null) {
                    lot.setValidBidsCount(lot.getBidsCount() - lot.getValidBidsCount());
                }
            }
        }

        if (cleanTender.getPublications() != null) {
            if (hasAnyIncludedPublication(isDesignContest()).test(cleanTender)) {
                cleanTender.setProcedureType(TenderProcedureType.DESIGN_CONTEST);
            } else if (hasAnyIncludedPublication(isApproachingBidders()).test(cleanTender)) {
                cleanTender.setProcedureType(TenderProcedureType.APPROACHING_BIDDERS);
            }
        }

        return cleanTender;
    }

    /**
     * @param predicate
     *      publication predicate
     * @return TRUE if at least one publication matches the given predicate.
     */
    private static Predicate<CleanTender> hasAnyIncludedPublication(final Predicate<Publication> predicate) {
        return t -> {
            if (t == null || t.getPublications() == null || predicate == null) {
                return false;
            }

            return t.getPublications().stream()
                .filter(p -> Boolean.TRUE.equals(p.getIsIncluded()))
                .anyMatch(predicate);
        };
    }

    /**
     * @return TRUE if the tested publication indicates DESIGN CONTEST
     */
    private static Predicate<Publication> isDesignContest() {
        return p -> p != null && Arrays.asList("MNA", "VNA").contains(p.getSourceFormType());
    }

    /**
     * @return TRUE if the tested publication indicates APPROACHING BIDDERS
     */
    private static Predicate<Publication> isApproachingBidders() {
        return p -> p != null && p.getSourceFormType() != null && p.getSourceFormType().startsWith("WY");
    }

    /**
     * @return supply type mapping for cleaning process
     */
    private static Map<Enum, List<String>> supplyTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(TenderSupplyType.SERVICES, Arrays.asList("Služby.", "Služby", "(c) Služby"));
        mapping.put(TenderSupplyType.WORKS, Arrays.asList("Práce.", "Stavebné práce.", "Stavebné práce", "(a) Práce."));
        mapping.put(TenderSupplyType.SUPPLIES, Arrays.asList("Tovary.", "Tovary", "(b) Tovary"));

        return mapping;
    }

    /**
     * @return procedure type mapping for cleaning process
     */
    private static Map<Enum, List<String>> procedureTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(TenderProcedureType.OPEN, Arrays.asList("Verejná súťaž", "Otvorená", "Otevřené", "Otvorené", "Otvorené (len na" +
            " skátenie lehoty)", "Výzva na predkladanie ponúk", "Otvorená Áno"));
        mapping.put(TenderProcedureType.RESTRICTED, Arrays.asList("Užšia súťaž", "Urychlené užší", "Užšia súťaž so skrátenými lehotami",
            "Omezené", "Užší", "Urychlené omezené", "Užšia súťaž so zrýchleným postupom", "Užšia súťaž zrýchleným postupom"));
        mapping.put(TenderProcedureType.COMPETITIVE_DIALOG, Arrays.asList("Súťažný dialóg", "Soutěžní dialog"));
        mapping.put(TenderProcedureType.NEGOTIATED, Arrays.asList("Vyjednávací", "Urychlené vyjednávací", "Urychlené jednací",
            "Rokovacie konanie", "Výzva na rokovanie/priame rokovacie konanie", "Rokovacie konanie zrýchleným postupom", "Výberové" +
            " konanie s rokovaním", "Výzva na predkladanie ponúk", "Priame rokovacie konanie"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITH_PUBLICATION, Arrays.asList("Rokovacie konanie so zverejnením", "Jednací s" +
            " uveřejněním", "Vyjednávací s výzvou k účasti v soutěži", "Rokovacie konanie s výzvou na súťaž", "Rokovacie konanie s " +
            "predchádzajúcou výzvou na súťaž", "Súťažné konanie s rokovaním", "Rokovacie konanie so zverejnením so skrátenými lehotami"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITHOUT_PUBLICATION, Arrays.asList("Rokovacie konanie bez zverejnenia",
            "Rokovacie konanie bez výzvy na súťaž", "Rokovacie konanie bez uverejnenia oznámenia o vyhlásení verejného obstarávania",
            "Vyjednávací bez zveřejnění oznámení o zakázce / výzvy k účasti v soutěži", "Vyjednací bez zveřejnění oznámení o zakázce /" +
            " výzvy k účasti v soutěži", "Rokovacie konanie bez uverejnenia oznámenia o vyhlásení verejného obstarávania/výzvy na súťaž",
            "Rokovacie konanie bez predchádzajúcej výzvy na súťaž (v súlade s článkom 50 smernice 2014/25/EÚ)", "Rokovacie konanie bez" +
            " predbežného zverejnenia (v súlade s článkom 32 smernice 2014/24/EÚ)"));
        mapping.put(TenderProcedureType.DESIGN_CONTEST, Arrays.asList("Súťaž návrhov"));
        mapping.put(TenderProcedureType.OUTRIGHT_AWARD, Arrays.asList("Zadání zakázky bez předchozího zveřejnění oznámení o zakázce v" +
            " Úředním věstníku Evropské unie (v případech uvedených v oddíle 2 přílohy D2)", "Zadání zakázky bez předchozího zveřejnění" +
            " oznámení o zakázce v Úředním věstníku Evropské unie (v případech uvedených v oddíle 2 přílohy D1)", "Zadání zakázky bez" +
            " předchozího zveřejnění oznámení o zakázce v Úředním věstníku Evropské unie (v případech uvedených v oddíle 2 přílohy D1," +
            " D2 nebo D3)", "Zadanie zákazky bez predchádzajúceho uverejnenia oznámenia o vyhlásení verejného obstarávania v Úradnom" +
            " vestníku Európskej únie (v prípadoch uvedených v písm. k) a l) v prílohe D)", "Zadanie zákazky bez predchádzajúceho" +
            " uverejnenia oznámenia o vyhlásení verejného obstarávania v Úradnom vestníku Európskej úunie (v prípadoch uvedených v písm." +
            " k) a l) v prílohe D)", "Zadanie zákazky bez predchádzajúceho uverejnenia oznámenia o vyhlásení verejného obstarávania v" +
            " Úradnom vestníku Európskej únie (v prípadoch uvedených v oddiele 2 prílohy D1) v prílohe D)", "Zadanie zákazky bez" +
            " predchádzajúceho uverejnenia oznámenia o vyhlásení verejného obstarávania v Úradnom vestníku Európskej únie (v prípadoch" +
            " uvedených v oddiele 2 prílohy D1) v prílohe D)", "Zadanie zákazky bez predchádzajúceho uverejnenia oznámenia o vyhlásení" +
            " verejného obstarávania v Úradnom vestníku Euróspkej únie (vyplňte prosím prílohu D)", "Pridelenie zákazky bez" +
            " predchádzajúceho uverejnenia oznámenia o vyhlásení verejného obstarávania v Úradnom vestníku Európskej únie v prípadoch" +
            " uvedených nižšie (vyplňte kompletne prílohu D2)", "Zadanie zákazky bez predchádzajúceho uverejnenia oznámenia o vyhlásení" +
            " verejného obstarávania v Úradnom vestníku Európskej únie (v prípadoch uvedených v oddiele 2 príloh D1, D2 prípadne D3)",
            "Odôvodnenie zadania zákazky bez predchádzajúceho uverejnenia oznámenia o vyhlásení verejného obstarávania v úradnom vestníku" +
            " Európskej únie (Ú. v. EÚ)", "Zadanie zákazky bez predchádzajúceho uverejnenia oznámenia o vyhlásení verejného obstarávania" +
            " v Ú. v. EÚ (v prípadoch uvedených v oddiele 2 prílohy D2)", "Zasdanie zákazky bez predchádzajúceho uverejnenia oznámenia o" +
            " vyhlásení verejného obstarávania v Úradnom vestníku Európskej únie (v prípadoch uvedených v písm. k) a l) v prílohe D)",
            "Zasdanie zákazky bez predchádzajúceho uverejnenia oznámenia o vyhlásení verejného obstarávania v Úradnom vestníkuc Európskej" +
            " úunie (v prípadoch uvedených v písm. k) a l) v prílohe D)", "Pridelenie zákazky bez predchádzajúceho uverejnenia oznámenia" +
            " o vyhlásení verejného obstarávania v Úradnom vestníku Európskej únie v prípadoch uvedených nižšie (vyplňte bod 2 v prílohe D)"
        ));
        mapping.put(TenderProcedureType.APPROACHING_BIDDERS, Arrays.asList("Zjednodušené podlimitní řízení"));

        return mapping;
    }

    /**
     * @return body type mapping
     */
    private static Map<Enum, List<String>> bodyTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(BuyerType.PUBLIC_BODY, Arrays.asList("Organizácia riadená verejným právom",
                "Verejný obstarávateľ (týka sa zákazky, na ktorú sa vzťahuje smernica 2004/18/ES)."));
        mapping.put(BuyerType.NATIONAL_AGENCY, Arrays.asList("Štátna agentúra/úrad"));
        mapping.put(BuyerType.NATIONAL_AUTHORITY, Arrays.asList(
                "Ministerstvo alebo iný štátny orgán vrátane regionálnych alebo miestnych útvarov",
                "Ministerstvo alebo iný štátny orgán vrátane jeho regionálnych alebo miestnych útvarov"));
        mapping.put(BuyerType.REGIONAL_AGENCY, Arrays.asList("Regionálna alebo miestna agentúra/úrad"));
        mapping.put(BuyerType.REGIONAL_AUTHORITY, Arrays.asList("Regionálny alebo miestny orgán"));
        mapping.put(BuyerType.OTHER, Arrays.asList("Iný verejný obstarávateľ"));

        return mapping;
    }

    /**
     * @return body activities mapping
     */
    private static Map<Enum, List<String>> bodyActivityMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(BuyerActivityType.GENERAL_PUBLIC_SERVICES, Arrays.asList("Všeobecné verejné služby", "Všeobecné " +
                "verejné služby Sociálna starostlivosť", "Verejný poriadok a bezpečnosť Všeobecné verejné služby"));
        mapping.put(BuyerActivityType.RECREATION_CULTURE_AND_RELIGION, Arrays.asList("Rekreácia, kultúra a " +
                "náboženstvo"));
        mapping.put(BuyerActivityType.HEALTH, Arrays.asList("Zdravotníctvo"));
        mapping.put(BuyerActivityType.EDUCATION, Arrays.asList("Vzdelávanie"));
        mapping.put(BuyerActivityType.ENVIRONMENT, Arrays.asList("Životné prostredie", "Všeobecné verejné služby " +
                "Životné prostredie"));
        mapping.put(BuyerActivityType.WATER, Arrays.asList("Vodárenstvo"));
        mapping.put(BuyerActivityType.DEFENCE, Arrays.asList("Obrana"));
        mapping.put(BuyerActivityType.SOCIAL_PROTECTION, Arrays.asList("Sociálna starostlivosť"));
        mapping.put(BuyerActivityType.PUBLIC_ORDER_AND_SAFETY, Arrays.asList("Verejný poriadok a bezpečnosť",
                "Verejný poriadok a bezpečnosť"));
        mapping.put(BuyerActivityType.URBAN_TRANSPORT, Arrays.asList("Služby mestskej železničnej, električkovej, " +
                "trolejbusovej alebo autobusovej dopravy"));
        mapping.put(BuyerActivityType.RAILWAY, Arrays.asList("Služby železničnej dopravy"));
        mapping.put(BuyerActivityType.ECONOMIC_AND_FINANCIAL_AFFAIRS, Arrays.asList("Hospodárstvo a finančné " +
                "záležitosti"));
        mapping.put(BuyerActivityType.ELECTRICITY, Arrays.asList("Elektrická energia"));
        mapping.put(BuyerActivityType.POSTAL, Arrays.asList("Poštové služby"));

        return mapping;
    }

    /**
     * @return form type mapping
     */
    private Map<Enum, List<String>> formTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();
        mapping.put(PublicationFormType.PRIOR_INFORMATION_NOTICE, Arrays.asList("POT", "POS", "POP", "POX"));
        mapping.put(PublicationFormType.CONTRACT_NOTICE, Arrays.asList(
                "MDP", "MDS", "MDT", "MNA", "MRP", "MRS", "MRT", "MSP", "MSS", "MST", "MUP", "MUS", "MUT",
                "WYP", "WYS", "WYT", "DEP", "DES", "DET", "WNP", "WNS", "WNT", "KPP", "KPS"));
        mapping.put(PublicationFormType.CONTRACT_AWARD, Arrays.asList(
                "IPP", "IPS", "IPT", "VBP", "VBS", "VBT", "VDP", "VDS", "VDT", "VEP", "VKP", "VKS", "VNA", "VNS",
                "VRP", "VRS", "VRT", "VSP", "VSS", "VST", "VUP", "VUS", "VUT", "ICP", "IEP", "IES", "IET", "IEX",
                "INO", "INP", "INS", "INT", "INX", "IVP", "IVS", "IVT", "IVX"));
        mapping.put(PublicationFormType.CONTRACT_CANCELLATION, Arrays.asList(
                "ZBP", "ZBS", "ZBT", "ZDP", "ZDS", "ZDT", "ZNA", "ZRP", "ZRS", "ZRT", "ZSP", "ZSS", "ZST", "ZUP",
                "ZUS", "ZUT", "ZWP", "ZWS", "ZWT"));
        mapping.put(PublicationFormType.CONTRACT_IMPLEMENTATION, Arrays.asList(
                "VZP", "VZS", "VZT"));
        mapping.put(PublicationFormType.OTHER, Arrays.asList(
                "IBP", "IBS", "IBT", "IDS", "IDT", "IDP", "INO", "IZP",
                "IZS", "IZT", "KOP", "KOS", "KSP", "KSS", "KST", "NSS", "PKT", "PRP",
                "PRS", "PRT"));
        mapping.put(PublicationFormType.CONTRACT_UPDATE, Arrays.asList(PublicationFormType.CONTRACT_UPDATE.name()));
        mapping.put(PublicationFormType.CONTRACT_AMENDMENT, Arrays.asList("DOP", "DOS", "DOT"));

        return mapping;
    }

    /**
     * @return form type mapping
     */
    private Map<Enum, List<String>> countryMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(CountryCode.AE, Arrays.asList("Spojené arabské emiráty"));
        mapping.put(CountryCode.AT, Arrays.asList("Rakúsko"));
        mapping.put(CountryCode.BE, Arrays.asList("Belgicko"));
        mapping.put(CountryCode.BG, Arrays.asList("Bulharsko"));
        mapping.put(CountryCode.CA, Arrays.asList("Kanada"));
        mapping.put(CountryCode.CH, Arrays.asList("Švajčiarsko"));
        mapping.put(CountryCode.CN, Arrays.asList("Čína"));
        mapping.put(CountryCode.CZ, Arrays.asList("Česká republika"));
        mapping.put(CountryCode.DE, Arrays.asList("Nemecko"));
        mapping.put(CountryCode.DK, Arrays.asList("Dánsko"));
        mapping.put(CountryCode.ES, Arrays.asList("Španielsko"));
        mapping.put(CountryCode.FR, Arrays.asList("Francúzsko"));
        mapping.put(CountryCode.GB, Arrays.asList("Spojené kráľovstvo", "Veľká Británia"));
        mapping.put(CountryCode.HU, Arrays.asList("Maďarsko"));
        mapping.put(CountryCode.IL, Arrays.asList("Izrael"));
        mapping.put(CountryCode.IT, Arrays.asList("Taliansko"));
        mapping.put(CountryCode.JP, Arrays.asList("Japonsko"));
        mapping.put(CountryCode.KP, Arrays.asList("Severná Kórea"));
        mapping.put(CountryCode.LK, Arrays.asList("Srí Lanka"));
        mapping.put(CountryCode.LT, Arrays.asList("Litva"));
        mapping.put(CountryCode.LU, Arrays.asList("Luxembursko"));
        mapping.put(CountryCode.NL, Arrays.asList("Holandsko"));
        mapping.put(CountryCode.NO, Arrays.asList("Nórsko"));
        mapping.put(CountryCode.PL, Arrays.asList("Poľsko"));
        mapping.put(CountryCode.PT, Arrays.asList("Portugalsko"));
        mapping.put(CountryCode.RO, Arrays.asList("Rumunsko"));
        mapping.put(CountryCode.RS, Arrays.asList("Srbsko"));
        mapping.put(CountryCode.RU, Arrays.asList("Rusko"));
        mapping.put(CountryCode.SE, Arrays.asList("Švédsko"));
        mapping.put(CountryCode.SG, Arrays.asList("Singapur"));
        mapping.put(CountryCode.SI, Arrays.asList("Slovinsko"));
        mapping.put(CountryCode.SK,
                Arrays.asList("Slovenská republika", "Slovensko", "Slovensko (SK)", "SR", "Slovemská republika", "SK"));
        mapping.put(CountryCode.SL, Arrays.asList("Sierra Leone"));
        mapping.put(CountryCode.SO, Arrays.asList("Somálsko"));
        mapping.put(CountryCode.US, Arrays.asList("Spojené štáty americké"));

        return mapping;
    }

    @Override
    protected final ParsedTender preProcessParsedItem(final ParsedTender parsedItem) {
        // Trim trailing zeros from sourceId to be united with already trimmed sourceIds
        if (parsedItem.getPublications() != null) {
            for (ParsedPublication publication : parsedItem.getPublications()) {
                if (publication.getSourceId() != null && !publication.getSourceId().isEmpty()) {
                    publication.setSourceId(publication.getSourceId().replaceAll("^0*", ""));
                }
            }
        }

        // remove dots from ends of address of implementation nuts
        if (parsedItem.getAddressOfImplementation() != null && parsedItem.getAddressOfImplementation().getNuts() != null) {
            final List<String> nuts = new ArrayList<>();

            for (String nut : parsedItem.getAddressOfImplementation().getNuts()) {
                nuts.add(nut.replaceAll("\\.$", ""));
            }

            parsedItem.getAddressOfImplementation().setNuts(nuts);
        }

        // replace SKK currency with EUR
        fixCurrency(parsedItem.getDocumentsPrice());
        fixCurrency(parsedItem.getEstimatedPrice());
        fixCurrency(parsedItem.getFinalPrice());
        if (parsedItem.getLots() != null) {
            for (ParsedTenderLot l : parsedItem.getLots()) {
                fixCurrency(l.getEstimatedPrice());
                fixCurrency(l.getRobustEstimatedPrice());

                if (l.getBids() != null) {
                    for (ParsedBid b : l.getBids()) {
                        fixCurrency(b.getPrice());
                        fixCurrency(b.getRobustPrice());
                        fixCurrency(b.getSubcontractedValue());

                        if (b.getUnitPrices() != null) {
                            for (ParsedUnitPrice p: b.getUnitPrices()) {
                                fixCurrency(p);
                            }
                        }

                        if (b.getPayments() != null) {
                            for (ParsedPayment p : b.getPayments()) {
                                fixCurrency(p.getPrice());
                            }
                        }
                    }
                }
            }
        }

        return parsedItem;
    }

    /**
     * If the currency of the given price is equal to SKK replace it with EUR.
     *
     * @param price
     *      price to be fixed
     */
    private static void fixCurrency(final BaseParsedPrice price) {
        if (price != null && "SKK".equals(price.getCurrency())) {
            price.setCurrency("EUR");
        }
    }
}
