package eu.datlab.worker.at.clean;

import eu.datlab.worker.clean.BaseDatlabTenderCleaner;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.codetables.TenderSupplyType;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.DatePlugin;
import eu.dl.worker.clean.plugin.IntegerPlugin;
import eu.dl.worker.clean.plugin.TenderProcedureTypePlugin;
import eu.dl.worker.clean.plugin.TenderSupplyTypePlugin;
import eu.dl.worker.clean.plugin.DateTimePlugin;
import eu.dl.worker.clean.plugin.BodyPlugin;
import eu.dl.worker.clean.plugin.PublicationPlugin;
import eu.dl.worker.clean.plugin.LotPlugin;
import eu.dl.worker.clean.plugin.PricePlugin;
import eu.dl.worker.clean.plugin.AddressPlugin;

import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.HashMap;


/**
 * Cleaner for data.gv.at tenders.
 *
 * @author Miroslav Brezik
 */
public final class DataGvTenderCleaner extends BaseDatlabTenderCleaner {
    private static final String VERSION = "1.0";

    private static final Locale LOCALE = new Locale("at");

    private static final NumberFormat NUMBER_FORMAT = NumberFormat.getInstance();
    private static final List<DateTimeFormatter> DATE_FORMATTERS = Arrays.asList(
            DateTimeFormatter.ISO_OFFSET_DATE,
            new DateTimeFormatterBuilder()
                .append(DateTimeFormatter.ISO_LOCAL_DATE)
                .toFormatter(LOCALE));

    private static final List<DateTimeFormatter> DATETIME_FORMATTERS = Arrays.asList(
            DateTimeFormatter.ISO_OFFSET_DATE_TIME,
            new DateTimeFormatterBuilder()
                .append(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
                .toFormatter(LOCALE));

    @Override
    protected void registerSpecificPlugins() {

        pluginRegistry
                .registerPlugin("integerPlugin", new IntegerPlugin(NUMBER_FORMAT))
                .registerPlugin("procedureType", new TenderProcedureTypePlugin(getProcedureTypeMapping()))
                .registerPlugin("supplyType", new TenderSupplyTypePlugin(getSupplyTypeMapping()))
                .registerPlugin("date", new DatePlugin<>(DATE_FORMATTERS))
                .registerPlugin("datetime", new DateTimePlugin<>(DATETIME_FORMATTERS))
                .registerPlugin("bodies", new BodyPlugin(null, null, null))
                .registerPlugin("publications", new PublicationPlugin(
                        NUMBER_FORMAT, DATETIME_FORMATTERS, getFormTypeMapping()))
                .registerPlugin("lots", new LotPlugin(NUMBER_FORMAT, DATE_FORMATTERS,  new HashMap<>()))
                .registerPlugin("prices", new PricePlugin(NUMBER_FORMAT))
                .registerPlugin("address", new AddressPlugin());
    }

    /**
     * @return form type mapping for cleaning
     */
    private Map<Enum, List<String>> getFormTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(PublicationFormType.PRIOR_INFORMATION_NOTICE, Arrays.asList("KD_8_1_Z1"));
        mapping.put(PublicationFormType.CONTRACT_NOTICE, Arrays.asList("KD_8_1_Z2", "KD_8_1_Z5"));
        mapping.put(PublicationFormType.CONTRACT_AWARD, Arrays.asList("KD_8_2_Z1", "KD_8_1_Z4"));
        mapping.put(PublicationFormType.CONTRACT_AMENDMENT, Arrays.asList("KD_8_2_Z3"));

        return mapping;
    }

    /**
     * @return supply type mapping for cleaning
     */
    private Map<Enum, List<String>> getSupplyTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderSupplyType.SUPPLIES, Arrays.asList("SUPPLIES"));
        mapping.put(TenderSupplyType.SERVICES, Arrays.asList("SERVICES"));
        mapping.put(TenderSupplyType.WORKS, Arrays.asList("WORKS"));

        return mapping;
    }

    /**
     * @return procedure type mapping for cleaning
     */
    private Map<Enum, List<String>> getProcedureTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderProcedureType.COMPETITIVE_DIALOG, Arrays.asList("PT_COMPETITIVE_DIALOGUE"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITH_PUBLICATION,
                Arrays.asList("PT_COMPETITIVE_NEGOTIATION", "PT_COMPETITIVE_NEGOTIATION,PT_WITH_PRIOR_NOTICE"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITHOUT_PUBLICATION,
                Arrays.asList("PT_COMPETITIVE_NEGOTIATION,PT_WITHOUT_PRIOR_NOTICE"));
        mapping.put(TenderProcedureType.OUTRIGHT_AWARD, Arrays.asList("PT_DIRECT", "PT_DIRECT,PT_WITH_PRIOR_NOTICE"));
        mapping.put(TenderProcedureType.INOVATION_PARTNERSHIP, Arrays.asList("PT_INNOVATION_PARTNERSHIP"));
        mapping.put(TenderProcedureType.OPEN, Arrays.asList("PT_OPEN"));
        mapping.put(TenderProcedureType.RESTRICTED,
                Arrays.asList("PT_RESTRICTED", "PT_RESTRICTED,PT_WITH_PRIOR_NOTICE"));
        mapping.put(TenderProcedureType.APPROACHING_BIDDERS,
                Arrays.asList("PT_RESTRICTED,PT_WITHOUT_PRIOR_NOTICE", "PT_SPECIAL_SERVICE",
                        "PT_SPECIAL_SERVICE,PT_WITHOUT_PRIOR_NOTICE", "PT_SPECIAL_SERVICE,PT_WITH_PRIOR_NOTICE"));

        return mapping;
    }

    @Override
    public String getVersion() {
        return VERSION;
    }

    @Override
    protected CleanTender postProcessSourceSpecificRules(final ParsedTender parsedItem, final CleanTender cleanItem) {
        return cleanItem;
    }

    @Override
    protected ParsedTender preProcessParsedItem(final ParsedTender parsedItem) {
        return parsedItem;
    }
}
