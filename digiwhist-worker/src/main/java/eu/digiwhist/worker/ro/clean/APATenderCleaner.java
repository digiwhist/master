package eu.digiwhist.worker.ro.clean;

import eu.digiwhist.worker.clean.BaseDigiwhistTenderCleaner;
import eu.digiwhist.worker.sk.clean.UvoTenderProcedureTypePlugin;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.CountryCode;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.BodyPlugin;
import eu.dl.worker.clean.plugin.LotPlugin;
import eu.dl.worker.clean.plugin.PricePlugin;
import eu.dl.worker.clean.plugin.PublicationPlugin;

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
public class APATenderCleaner extends BaseDigiwhistTenderCleaner {
    private static final String VERSION = "1";

    private static final NumberFormat NUMBER_FORMAT = NumberFormat.getInstance(new Locale("ro"));

    private static final List<DateTimeFormatter> DATE_FORMATTERS = Arrays.asList(
            DateTimeFormatter.ofPattern("uuuu-MM-dd hh:mm:ss"));

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
                        new UvoTenderProcedureTypePlugin(procedureTypeMapping(), null));
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
        parsedItem.getPublications().forEach(
                t -> t.setPublicationDate(t.getPublicationDate().replaceAll("\\..*", "")));
        parsedItem.getLots().forEach(
                t -> t.setContractSignatureDate(t.getContractSignatureDate().replaceAll("\\..*", "")));
        return parsedItem;
    }
}
