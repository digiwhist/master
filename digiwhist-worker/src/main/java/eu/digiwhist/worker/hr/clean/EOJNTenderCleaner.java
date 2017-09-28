package eu.digiwhist.worker.hr.clean;

import eu.digiwhist.worker.clean.BaseDigiwhistTenderCleaner;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
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
public class EOJNTenderCleaner extends BaseDigiwhistTenderCleaner {
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
                .registerPlugin("bodies", new BodyPlugin(null, getBuyerActivityMapping()))
                .registerPlugin("address", new AddressPlugin())
                .registerPlugin("lots", new LotPlugin(NUMBER_FORMAT, DATE_FORMATTERS, new HashMap<>()))
                .registerPlugin("publications",
                        new PublicationPlugin(NUMBER_FORMAT, DATE_FORMATTERS, getFormTypeMapping()))
                .registerPlugin("procedureType", new TenderProcedureTypePlugin(getProcedureTypeMapping()))
                .registerPlugin("prices", new PricePlugin(NUMBER_FORMAT))
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

        // todo: implement when task #3726 is done

        return mapping;
    }

    /**
     * @return form type mapping
     */
    private Map<Enum, List<String>> getFormTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        // todo: implement when task #3729 is done
        mapping.put(PublicationFormType.CONTRACT_NOTICE, Arrays.asList("Poziv na nadmetanje", "POZIV NA NADMETANJE",
                "Poziv na nadmetanje – sektor", "POZIV NA NADMETANJE - SEKTOR"));
        mapping.put(PublicationFormType.CONTRACT_AWARD, Arrays.asList("Obavijest o sklopljenim ugovorima",
                "OBAVIJEST O SKLOPLJENIM UGOVORIMA", "Obavijest o sklopljenim ugovorima - sektor",
                "OBAVIJEST O SKLOPLJENIM UGOVORIMA - SEKTORSKI"));

        return mapping;
    }

    /**
     * @return procedure type mapping for cleaning process
     */
    private static Map<Enum, List<String>> getProcedureTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        // todo: implement when task #3730 is done

        return mapping;
    }
}
