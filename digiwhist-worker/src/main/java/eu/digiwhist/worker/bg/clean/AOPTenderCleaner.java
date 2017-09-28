package eu.digiwhist.worker.bg.clean;

import eu.digiwhist.worker.clean.BaseDigiwhistTenderCleaner;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.BuyerActivityType;
import eu.dl.dataaccess.dto.codetables.BuyerType;
import eu.dl.dataaccess.dto.codetables.CountryCode;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
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
import eu.dl.worker.clean.plugin.PricePlugin;
import eu.dl.worker.clean.plugin.PublicationPlugin;
import eu.dl.worker.clean.plugin.TenderSupplyTypePlugin;

import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Created by michalriha on 10/07/2017.
 */
public class AOPTenderCleaner extends BaseDigiwhistTenderCleaner {
    private static final String VERSION = "1.0";
    private static final NumberFormat NUMBER_FORMAT = NumberFormat.getInstance(new Locale("bg"));
    private static final DateTimeFormatter DATETIME_FORMATTER = DateTimeFormatter.ofPattern("dd/MM/uuuu ");
    private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("dd/MM/uuuu");

    @SuppressWarnings("unchecked")
    @Override
    protected final void registerSpecificPlugins() {
        Map<String, Map<Enum, List<String>>> lotMappings = new HashMap<>();
        lotMappings.put("countryMapping", countryMapping());

        pluginRegistry
                .registerPlugin("integerPlugin", new IntegerPlugin(NUMBER_FORMAT))
                .registerPlugin("date", new DatePlugin(DATETIME_FORMATTER))
                .registerPlugin("datetime", new DateTimePlugin(DATETIME_FORMATTER))
                .registerPlugin("supplyType", new TenderSupplyTypePlugin(supplyTypeMapping()))
                .registerPlugin("bodies", new BodyPlugin(bodyTypeMapping(), bodyActivityMapping(), countryMapping()))
                .registerPlugin("lots", new LotPlugin(NUMBER_FORMAT, DATE_FORMATTER, lotMappings))
                .registerPlugin("prices", new PricePlugin(NUMBER_FORMAT))
                .registerPlugin("fundings", new FundingsPlugin(NUMBER_FORMAT))
                .registerPlugin("address", new AddressPlugin())
                .registerPlugin("awardCriteria", new AwardCriteriaPlugin(NUMBER_FORMAT))
                .registerPlugin("publications",
                        new PublicationPlugin(NUMBER_FORMAT, DATE_FORMATTER, formTypeMapping()));
    }

    /**
     * @return form type mapping
     */
    private Map<Enum, List<String>> formTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(PublicationFormType.CONTRACT_AWARD, Arrays.asList("Обявление за възложена поръчка", "ОБЯВЛЕНИЕ" +
                        " ЗА ВЪЗЛОЖЕНА ПОРЪЧКА", "ОБЯВЛЕНИЕ ЗА ВЪЗЛОЖЕНА ПОРЪЧКА",
                "ОБЯВЛЕНИЕ ЗА ВЪЗЛОЖЕНА ПОРЪЧКА В ОБЛАСТТА " +
                        "НА ОТБРАНАТА И СИГУРНОСТТА", "Обявление за възложена поръчка - комунални услуги",
                "ОБЯВЛЕНИЕ ЗА ВЪЗЛО" +
                        "ЖЕНА ПОРЪЧКА - СПЕЦИАЛНИ СЕКТОРИ", "ОБЯВЛЕНИЕ ЗА ВЪЗЛОЖЕНА ПОРЪЧКА - СПЕЦИАЛНИ СЕКТОРИ",
                "ОБЯВЛЕНИЕ З" +
                        "А МАЛКА ОБЩЕСТВЕНА ПОРЪЧКА", "РЕШЕНИЕ"));
        mapping.put(PublicationFormType.CONTRACT_NOTICE, Arrays.asList("ОБЯВЛЕНИЕ ЗА КОНКУРС ЗА ПРОЕКТ", "ОБЯВЛЕНИЕ " +
                        "ЗА ОБЩЕСТВЕНА ПОРЪЧКА", "ОБЯВЛЕНИЕ ЗА ОБЩЕСТВЕНА ПОРЪЧКА ОТ ВЪЗЛОЖИТЕЛ ПО ЧЛ. 7, Т. 5 ИЛИ 6 " +
                        "ОТ ЗОП",
                "Обявление за поръчка", "ОБЯВЛЕНИЕ ЗА ПОРЪЧКА", "ОБЯВЛЕНИЕ ЗА ПОРЪЧКА", "ОБЯВЛЕНИЕ ЗА ПОРЪЧКА В " +
                        "ОБЛАСТТА НА ОТБРАНАТА И СИГУРНОСТТА", "Обявление за поръчка - комунални услуги", "ОБЯВ" +
                        "ЛЕНИЕ ЗА ПОРЪЧКА - СПЕЦИАЛНИ СЕКТОРИ", "ОБЯВЛЕНИЕ ЗА ПОРЪЧКА — СПЕЦИАЛНИ СЕКТОРИ"));
        mapping.put(PublicationFormType.CONTRACT_CANCELLATION, Arrays.asList("Обявление за приключване на догово" +
                "р за обществена поръчка"));
        mapping.put(PublicationFormType.CONTRACT_IMPLEMENTATION, Arrays.asList("ИНФОРМАЦИЯ ЗА ИЗПЪЛНЕНИЕТО НА ДОГО" +
                "ВОР ЗА ОБЩЕСТВЕНА ПОРЪЧКА"));
        mapping.put(PublicationFormType.PRIOR_INFORMATION_NOTICE, Arrays.asList("ИНФОРМАЦИЯ ЗА ПРОВЕДЕН КОНКУРС ЗА " +
                        "ПРОЕКТ", "ИНФОРМАЦИЯ ЗА СКЛЮЧЕН ДОГОВОР", "ИНФОРМАЦИЯ ЗА СКЛЮЧЕН ДОГОВОР ЗА МАЛКА ОБЩЕСТВЕНА" +
                        " " +
                        "ПОРЪЧКА", "ИНФОРМАЦИЯ ЗА СКЛЮЧЕН ДОГОВОР ОТ ВЪЗЛОЖИТЕЛ ПО ЧЛ. 7, Т. 5 ИЛИ 6 ОТ ЗОП",
                "Решение за откриване на процедура"));
        mapping.put(PublicationFormType.OTHER, Arrays.asList("ИНФОРМАЦИЯ ЗА ХОДА НА ПРОЦЕДУРАТА ПРИ ПРОИЗВОДСТВО ПО " +
                        "ОБЖАЛВАНЕ", "Информация при производство по обжалване", "КВАЛИФИКАЦИОННА СИСТЕМА — СПЕЦИАЛНИ" +
                        " " +
                        "СЕКТОРИ", "КВАЛИФИКАЦИОННА СИСТЕМА — СПЕЦИАЛНИ СЕКТОРИ", "ОБЯВЛЕНИЕ ЗА ДОБРОВОЛНА ПРОЗРАЧ" +
                        "НОСТ EX " +
                        "ANTE", "ОБЯВЛЕНИЕ ЗА СИСТЕМА ЗА ПРЕДВАРИТЕЛЕН ПОДБОР ОТ ВЪЗЛОЖИТЕЛ ПО ЧЛ. 7, Т. 5 ИЛИ 6 ОТ " +
                        "ЗОП",
                "РЕЗУЛТАТИ ОТ КОНКУРС ЗА ПРОЕКТ", "Социални и други специфични услуги - обществени поръчки"));

        return mapping;
    }

    /**
     * @return body activity mapping
     */
    private Map<Enum, List<String>> bodyActivityMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(BuyerActivityType.WATER, Arrays.asList("Вода"));
        mapping.put(BuyerActivityType.URBAN_TRANSPORT, Arrays.asList("Градски железопътни, трамвайни, тролейбусни " +
                "или автобусни услуги", "Поддържане на жп инфраструктур"));
        mapping.put(BuyerActivityType.GENERAL_PUBLIC_SERVICES, Arrays.asList("CTOПAHИCBAHE, ПOДДЪPЖAHE И OTДABAH" +
                "E ПOД HAEM HA OTKPИTИ И ЗAKPИTИ TЪPГOBCKИ ПЛOЩИ И CЪOPЪЖEHИЯ"));
        mapping.put(BuyerActivityType.HEALTH, Arrays.asList("Kонтрол по безопасност на храните", "областта на " +
                "пристанищата", "здравеопазване"));
        mapping.put(BuyerActivityType.GENERAL_PUBLIC_SERVICES, Arrays.asList("административни"));
        mapping.put(BuyerActivityType.ENVIRONMENT, Arrays.asList("Горско стопанство", "СТопанисване на горите"));
        mapping.put(BuyerActivityType.ELECTRICITY, Arrays.asList("Електро", "Енергетика"));
        mapping.put(BuyerActivityType.EDUCATION, Arrays.asList("образование", "Общинска Администрация"));

        return mapping;
    }

    /**
     * @return body type mapping
     */
    private Map<Enum, List<String>> bodyTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(BuyerType.REGIONAL_AUTHORITY, Arrays.asList("районна администрация"));

        return mapping;
    }

    /**
     * @return supply type mapping
     */
    private Map<Enum, List<String>> supplyTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderSupplyType.SERVICES, Arrays.asList("Услуги"));
        mapping.put(TenderSupplyType.WORKS, Arrays.asList("Строителство"));
        mapping.put(TenderSupplyType.SUPPLIES, Arrays.asList("Доставки"));

        return mapping;
    }

    /**
     * @return country mapping
     */
    private Map<Enum, List<String>> countryMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(CountryCode.VE, Collections.singletonList("Венецуела"));
        mapping.put(CountryCode.BR, Collections.singletonList("Бразилия"));
        mapping.put(CountryCode.BF, Collections.singletonList("Буркина Фасо"));
        mapping.put(CountryCode.GM, Collections.singletonList("Гамбия"));
        mapping.put(CountryCode.VG, Collections.singletonList("Британски Вирджински о-ви"));
        mapping.put(CountryCode.ZM, Collections.singletonList("Замбия"));
        mapping.put(CountryCode.GY, Collections.singletonList("Гвиана"));
        mapping.put(CountryCode.BM, Collections.singletonList("Бермуда"));
        mapping.put(CountryCode.GT, Collections.singletonList("Гватемала"));
        mapping.put(CountryCode.RU, Collections.singletonList("Руска федерация"));
        mapping.put(CountryCode.VA, Collections.singletonList("Ватикана"));
        mapping.put(CountryCode.BI, Collections.singletonList("Бурунди"));
        mapping.put(CountryCode.GA, Collections.singletonList("Габон"));
        mapping.put(CountryCode.GE, Collections.singletonList("Грузия"));
        mapping.put(CountryCode.VN, Collections.singletonList("Виетнам"));
        mapping.put(CountryCode.GN, Collections.singletonList("Гвинея"));
        mapping.put(CountryCode.BS, Collections.singletonList("Бахамите"));
        mapping.put(CountryCode.GW, Collections.singletonList("Гвинея Бисау"));
        mapping.put(CountryCode.IO, Collections.singletonList("Британска територия в Индийския океан"));
        mapping.put(CountryCode.GI, Collections.singletonList("Гибралтар"));
        mapping.put(CountryCode.EE, Collections.singletonList("Естония"));
        mapping.put(CountryCode.GP, Collections.singletonList("Гваделупа"));
        mapping.put(CountryCode.BO, Collections.singletonList("Боливия"));
        mapping.put(CountryCode.VU, Collections.singletonList("Вануату"));
        mapping.put(CountryCode.DE, Collections.singletonList("Германия"));
        mapping.put(CountryCode.BG, Collections.singletonList("България"));
        mapping.put(CountryCode.GH, Collections.singletonList("Гана"));
        mapping.put(CountryCode.BT, Collections.singletonList("Бутан"));

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
        return parsedItem;
    }
}
