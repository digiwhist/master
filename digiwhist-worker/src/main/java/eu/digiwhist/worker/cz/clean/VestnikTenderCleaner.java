package eu.digiwhist.worker.cz.clean;

import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.BuyerActivityType;
import eu.dl.dataaccess.dto.codetables.NpwpReason;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.codetables.TenderSupplyType;
import eu.dl.dataaccess.dto.parsed.ParsedTender;

import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.temporal.ChronoField;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Cleaner for the cz - vestnik source.
 */
public final class VestnikTenderCleaner extends BaseVestnikTenderCleaner {
    private static final List<DateTimeFormatter> DATE_FORMATTERS = Arrays.asList(
            DateTimeFormatter.ofPattern("d/M/uuuu"), DateTimeFormatter.ofPattern("d. M. uuuu"),
            DateTimeFormatter.ofPattern("d.M.uuuu"), DateTimeFormatter.ofPattern("uuuu/M/d"));

    private static final List<DateTimeFormatter> DATETIME_FORMATTERS = Arrays.asList(
            new DateTimeFormatterBuilder().appendPattern("d/M/uuuu[ H:m]")
                    .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
                    .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
                    .toFormatter(),
            new DateTimeFormatterBuilder().appendPattern("d.M.uuuu[ H:m]")
                    .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
                    .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
                    .toFormatter());

    private static final List<String> DESIGN_CONTEST_SOURCE_FORM_TYPES = Arrays.asList("12", "13");

    @Override
    protected Map<Enum, List<String>> getBodyActivityMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(BuyerActivityType.DEFENCE, Arrays.asList("Obrana"));
        mapping.put(BuyerActivityType.ECONOMIC_AND_FINANCIAL_AFFAIRS,
                Arrays.asList("Hospodářské a finanční záležitosti"));
        mapping.put(BuyerActivityType.EDUCATION, Arrays.asList("Školství"));
        mapping.put(BuyerActivityType.ENVIRONMENT, Arrays.asList("Životní prostředí", "lesnictví",
                "Hospodaření s majetkem státu v působnosti MO v oblasti lesního hospodářství",
                "Hospodaření s majetkem státu v působnosti MO v oblasti lesní, zemědělské a dřevozpracující."));
        mapping.put(BuyerActivityType.GENERAL_PUBLIC_SERVICES,
                Arrays.asList("Služby pro širokou veřejnost", "stavba silnic a dálnic",
                        "územně samosprávný celek a z toho plynoucí hlavní činnost"));
        mapping.put(BuyerActivityType.HEALTH, Arrays.asList("Zdravotnictví"));
        mapping.put(BuyerActivityType.HOUSING_AND_COMMUNITY_AMENITIES, Arrays.asList("Bydlení a občanská vybavenost"));
        mapping.put(BuyerActivityType.PUBLIC_ORDER_AND_SAFETY, Arrays.asList("Veřejný pořádek a bezpečnost"));
        mapping.put(BuyerActivityType.RECREATION_CULTURE_AND_RELIGION,
                Arrays.asList("Rekreace, kultura a náboženství"));
        mapping.put(BuyerActivityType.AIRPORT,
                Arrays.asList("Řízení letového provozu", "Činnosti související s letišti"));
        mapping.put(BuyerActivityType.ELECTRICITY, Arrays.asList("Elektřina"));
        mapping.put(BuyerActivityType.COAL_AND_OTHER_EXTRACTION,
                Arrays.asList("Vyhledávání a těžba uhlí a dalších pevných paliv"));
        mapping.put(BuyerActivityType.GAS_AND_HEAT_PRODUCTION,
                Arrays.asList("Výroba, přeprava a distribuce plynu a tepla"));
        mapping.put(BuyerActivityType.RAILWAY, Arrays.asList("Železniční služby"));
        mapping.put(BuyerActivityType.URBAN_TRANSPORT,
                Arrays.asList("Městská železniční, tramvajová, trolejbusová nebo autobusová doprava",
                        "Příměstská železniční, tramvajová, trolejbusová nebo autobusová doprava"));
        mapping.put(BuyerActivityType.WATER,
                Arrays.asList("Vodohospodářství", "správa povodí", "výkon správy povodí", "Výkon správy povodí"));
        mapping.put(BuyerActivityType.POSTAL, Arrays.asList("Poštovní služby"));
        mapping.put(BuyerActivityType.GAS_AND_OIL_EXTRACTION,
                Arrays.asList("Vyhledávání a těžba zemního plynu a ropy"));
        mapping.put(BuyerActivityType.PORT, Arrays.asList("Činnosti související s přístavy"));
        mapping.put(BuyerActivityType.SOCIAL_PROTECTION, Arrays.asList("Sociální služby"));

        return mapping;
    }

    @Override
    protected Map<Enum, List<String>> getSupplyTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderSupplyType.SERVICES, Arrays.asList("Služby"));
        mapping.put(TenderSupplyType.WORKS, Arrays.asList("Stavební práce"));
        mapping.put(TenderSupplyType.SUPPLIES, Arrays.asList("Dodávky"));

        return mapping;
    }

    @Override
    protected Map<Enum, List<String>> getFormTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(PublicationFormType.PRIOR_INFORMATION_NOTICE,
                Arrays.asList("1", "4", "15", "16", "Oznámení předběžných informací",
                        "Oznámení o dobrovolné průhlednosti ex ante"));
        mapping.put(PublicationFormType.CONTRACT_NOTICE, Arrays.asList("2", "5", "9", "12", "17", "Oznámení o zakázce",
                "Zjednodušené oznámení o zakázce v rámci dynamického nákupního systému"));
        mapping.put(PublicationFormType.CONTRACT_AWARD, Arrays.asList("3", "6", "13", "18"));
        mapping.put(PublicationFormType.CONTRACT_IMPLEMENTATION, Arrays.asList("54"));
        mapping.put(PublicationFormType.CONTRACT_CANCELLATION, Arrays.asList("51"));
        mapping.put(PublicationFormType.OTHER, Arrays.asList("Oznámení na profilu kupujícího"));

        return mapping;
    }

    @Override
    protected List<DateTimeFormatter> getDateFormatters() {
        return DATE_FORMATTERS;
    }

    @Override
    protected Map<Enum, List<String>> getProcedureTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderProcedureType.OPEN, Arrays.asList("Otevřené"));
        mapping.put(TenderProcedureType.RESTRICTED,
                Arrays.asList("Urychlené omezené", "Urychlené užší", "Omezené", "Užší"));
        mapping.put(TenderProcedureType.NEGOTIATED,
                Arrays.asList("Urychlené vyjednávací", "Urychlené jednací", "Vyjednávací",
                        "Vyjednávací s výzvou k účasti v soutěži"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITHOUT_PUBLICATION,
                Arrays.asList("Vyjednávací bez zveřejnění oznámení o zakázce",
                        "Vyjednávací bez zveřejnění oznámení o zakázce / výzvy k účasti v soutěži",
                        "Vyjednací bez zveřejnění oznámení o zakázce / výzvy k účasti v soutěži"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITH_PUBLICATION,
                Arrays.asList("Jednací s uveřejněním", "Vyjednávací se zveřejněním oznámení o zakázce"));
        mapping.put(TenderProcedureType.APPROACHING_BIDDERS,
                Arrays.asList("Zjednodušené podlimitní řízení"));
        mapping.put(TenderProcedureType.COMPETITIVE_DIALOG, Arrays.asList("Soutěžní dialog"));
        mapping.put(TenderProcedureType.OUTRIGHT_AWARD, Arrays.asList(
                "Zadání zakázky bez předchozího zveřejnění oznámení o zakázce v Úředním věstníku " + "Evropské unie "
                        + "(v případech uvedených v oddíle 2 přílohy D1)",
                "Zadání zakázky bez předchozího zveřejnění oznámení o zakázce v Úředním věstníku " + "Evropské unie "
                        + "(v případech uvedených v oddíle 2 přílohy D2)",
                "Zadání zakázky bez předchozího zveřejnění oznámení o zakázce v Úředním věstníku " + "Evropské unie "
                        + "(v případech uvedených v oddíle 2 přílohy D1, D2 nebo D3)"));

        return mapping;
    }

    @Override
    protected List<DateTimeFormatter> getDateTimeFormatters() {
        return DATETIME_FORMATTERS;
    }

    @Override
    protected List<String> getDesignContestSourceFormTypes() {
        return DESIGN_CONTEST_SOURCE_FORM_TYPES;
    }

    @Override
    protected Map<Enum, List<String>> getNpwpReasonMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(NpwpReason.NO_VALID_OFFERS_IN_PRECEEDING_PROCUREMENT,
                Arrays.asList("AttItems.ZadneVhodneNabidky_1", "AttItems.ZadnazNabidekNebylaPrijatelna_1"));
        mapping.put(NpwpReason.RESEARCH_PROCUREMENT,
                Collections.singletonList("AttItems.ZahrnuteVyrobkyZaUcelemVyzkumu_1"));
        mapping.put(NpwpReason.TECHNICAL_EXCLUSIVITY,
                Collections.singletonList("AttItems.PouzeKonkretnimDodavatelemTechnicke_1"));
        mapping.put(NpwpReason.ART_EXCLUSIVITY,
                Collections.singletonList("AttItems.PouzeKonkretnimDodavatelemUmelecke_1"));
        mapping.put(NpwpReason.AUTHORSHIP_RIGHTS_EXCLUSIVITY,
                Collections.singletonList("AttItems.PouzeKonkretnimDodavatelem_1"));
        mapping.put(NpwpReason.EMERGENCY, Collections.singletonList("AttItems.KrajniNalehavost_1"));
        mapping.put(NpwpReason.ADDITIONAL_WORK,
                Arrays.asList("AttItems.DalsiStavebniPraceVsouladu_1", "AttItems.NoveStavebniPrace_1"));
        mapping.put(NpwpReason.PROPOSAL_CONTEST_FOLLOW_UP,
                Collections.singletonList("AttItems.ZakazkaZadanaSouteziNaUrcityVykon_1"));
        mapping.put(NpwpReason.COMMODITY_MARKET, Collections.singletonList("AttItems.ZboziKotovaneNaTrhuKomodit_1"));
        mapping.put(NpwpReason.ADVANTAGEOUS_CONDITIONS,
                Arrays.asList("AttItems.NakupZboziZaObzvlasteVyhodnychPodminek_1",
                        "AttItems.NakupZboziOdKonkurznichSpravcu_1"));
        mapping.put(NpwpReason.FRAMEWORK_AGREEMENT,
                Collections.singletonList("AttItems.ZakazkyPridelenyNaZakladeRS_1"));
        mapping.put(NpwpReason.OUTSIDE_DIRECTIVE, Collections.singletonList("AttItems.JineOduvodneniZadaniZakazky_1"));

        return mapping;
    }

    @Override
    protected ParsedTender preProcessParsedItem(final ParsedTender parsedItem) {
        return parsedItem;
    }

    @Override
    protected CleanTender postProcessVestnikOrVvzSpecificRules(final ParsedTender parsedTender,
            final CleanTender cleanTender) {
        // nothing specific to do
        return cleanTender;
    }
}
