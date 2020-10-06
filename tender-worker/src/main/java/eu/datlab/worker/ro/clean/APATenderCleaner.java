package eu.datlab.worker.ro.clean;

import eu.datlab.worker.clean.BaseDatlabTenderCleaner;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.CountryCode;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.SelectionMethod;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.codetables.TenderSupplyType;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.AwardCriteriaPlugin;
import eu.dl.worker.clean.plugin.BodyPlugin;
import eu.dl.worker.clean.plugin.DatePlugin;
import eu.dl.worker.clean.plugin.DateTimePlugin;
import eu.dl.worker.clean.plugin.LotPlugin;
import eu.dl.worker.clean.plugin.PricePlugin;
import eu.dl.worker.clean.plugin.PublicationPlugin;
import eu.dl.worker.clean.plugin.SelectionMethodPlugin;
import eu.dl.worker.clean.plugin.TenderProcedureTypePlugin;
import eu.dl.worker.clean.plugin.TenderSupplyTypePlugin;

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Cleaner for Romanian tenders.
 */
public class APATenderCleaner extends BaseDatlabTenderCleaner {
    private static final String VERSION = "1";

    private static final Locale LOCALE = new Locale("ro");

    private static final NumberFormat NUMBER_FORMAT;
    static {
        DecimalFormatSymbols formatSymbols = new DecimalFormatSymbols(LOCALE);
        formatSymbols.setDecimalSeparator('.');
        formatSymbols.setGroupingSeparator(',');
        NUMBER_FORMAT = new DecimalFormat("#,##0.###", formatSymbols);
    }

    private static final List<DateTimeFormatter> DATE_FORMATTERS = Arrays.asList(
        DateTimeFormatter.ofPattern("uuuu-MM-dd hh:mm:ss[.SSS]"),
        DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm:ss[.SSSSSSSSS]"),
        DateTimeFormatter.ofPattern("dd.MM.uuuu HH:mm:ss[.SSSSSSSSS]"),
        DateTimeFormatter.ofPattern("dd-MM-uuuu HH:mm:ss[.SSSSSSSSS]")
    );

    @SuppressWarnings("unchecked")
    @Override
    protected final void registerSpecificPlugins() {
        Map<String, Map<Enum, List<String>>> lotMappings = new HashMap<>();
        lotMappings.put("countryMapping", countryMapping());

        pluginRegistry
                .registerPlugin("lots", new LotPlugin(NUMBER_FORMAT, DATE_FORMATTERS, lotMappings))
                .registerPlugin("prices", new PricePlugin(NUMBER_FORMAT))
                .registerPlugin("bodies", new BodyPlugin(null, null, null))
                .registerPlugin("publications", new PublicationPlugin(NUMBER_FORMAT, DATE_FORMATTERS,
                        formTypeMapping()))
                .registerPlugin("procedureType",
                        new TenderProcedureTypePlugin(procedureTypeMapping(), null))
                .registerPlugin("supplyType", new TenderSupplyTypePlugin(supplyTypeMapping()))
                .registerPlugin("selectionMethod", new SelectionMethodPlugin(selectionMethodMapping()))
                .registerPlugin("date", new DatePlugin(DATE_FORMATTERS))
                .registerPlugin("datetime", new DateTimePlugin(DATE_FORMATTERS))
                .registerPlugin("awardCriterion", new AwardCriteriaPlugin(NUMBER_FORMAT));
    }

    /**
     * @return supply type mapping
     */
    private Map<Enum, List<String>> supplyTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderSupplyType.SUPPLIES, Arrays.asList("Furnizare"));
        mapping.put(TenderSupplyType.SERVICES, Arrays.asList("Servicii"));
        mapping.put(TenderSupplyType.WORKS, Arrays.asList("Lucrari"));

        return mapping;
    }

    /**
     * @return form type mapping
     */
    private Map<Enum, List<String>> formTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(PublicationFormType.CONTRACT_NOTICE, Arrays.asList("CONTRACT_NOTICE"));
        mapping.put(PublicationFormType.CONTRACT_AWARD, Arrays.asList("CONTRACT_AWARD", "OUTRIGHT_AWARD"));

        return mapping;
    }

    /**
     * @return country mapping
     */
    private Map<Enum, List<String>> countryMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(CountryCode.IE, Arrays.asList("Ireland"));
        mapping.put(CountryCode.TR, Arrays.asList("Turkey"));
        mapping.put(CountryCode.US, Arrays.asList("United States"));
        mapping.put(CountryCode.DE, Arrays.asList("Germany"));
        mapping.put(CountryCode.PL, Arrays.asList("Poland"));
        mapping.put(CountryCode.HU, Arrays.asList("Hungary"));
        mapping.put(CountryCode.FR, Arrays.asList("France"));
        mapping.put(CountryCode.NL, Arrays.asList("Netherlands"));
        mapping.put(CountryCode.GB, Arrays.asList("United Kingdom"));
        mapping.put(CountryCode.RO, Arrays.asList("Romania"));
        mapping.put(CountryCode.CZ, Arrays.asList("Czech Republic"));
        mapping.put(CountryCode.IT, Arrays.asList("Italy"));
        mapping.put(CountryCode.ES, Arrays.asList("Spain"));
        mapping.put(CountryCode.CA, Arrays.asList("Canada"));
        mapping.put(CountryCode.BG, Arrays.asList("Bulgaria"));

        return mapping;
    }

    /**
     * @return procedure type mapping for cleaning process
     */
    private static Map<Enum, List<String>> procedureTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderProcedureType.PUBLIC_CONTEST, Arrays.asList("Anunt de atribuire la anunt de participare"));
        mapping.put(TenderProcedureType.OUTRIGHT_AWARD, Arrays.asList("Cumparare directa"));
        mapping.put(TenderProcedureType.APPROACHING_BIDDERS, Arrays.asList("Invitatie de participare",
                "Procedura simplificata", "Cerere de oferta", "Licitatie deschisa accelerata"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITHOUT_PUBLICATION, Arrays.asList("Negociere fara anunt de participare"));
        mapping.put(TenderProcedureType.OPEN, Arrays.asList("Licitatie deschisa"));
        mapping.put(TenderProcedureType.NEGOTIATED, Arrays.asList("Negociere", "Negociere accelerata",
                "Procedura competitiva cu negociere"));
        mapping.put(TenderProcedureType.RESTRICTED, Arrays.asList("Licitatie restransa", "Licitatie restransa accelerata"));
        mapping.put(TenderProcedureType.COMPETITIVE_DIALOG, Arrays.asList("Dialog competitiv"));
        return mapping;
    }

    /**
     * @return selection method mapping
     */
    private Map<Enum, List<String>> selectionMethodMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(SelectionMethod.LOWEST_PRICE, Arrays.asList("Pretul cel mai scazut", "Costul cel mai scazut"));
        mapping.put(SelectionMethod.MEAT, Arrays.asList("Oferta cea mai avantajoasa d.p.d.v. economic",
                "Cel mai bun raport calitate – pret", "Cel mai bun raport calitate-pret",
                "Cel mai bun raport calitate – cost", "Cel mai bun raport calitate–cost", "CRITERIU_ATRIBUIRE",
                "Cel mai bun raport calitate�cost"));
        return mapping;
    }


    @Override
    public final String getVersion() {
        return VERSION;
    }

    @Override
    protected final CleanTender postProcessSourceSpecificRules(final ParsedTender parsedItem, final CleanTender
            cleanItem) {
        return cleanItem;
    }

    @Override
    protected final ParsedTender preProcessParsedItem(final ParsedTender parsedItem) {
        parsedItem.getPublications().stream().filter(p -> p.getPublicationDate().matches("....-.*")).forEach(
                t -> t.setPublicationDate(t.getPublicationDate().replaceAll("\\..*", "")));
        parsedItem.getLots().stream().filter(l -> l.getContractSignatureDate().matches("....-.*")).forEach(
                t -> t.setContractSignatureDate(t.getContractSignatureDate().replaceAll("\\..*", "")));

        return parsedItem;
    }
}
