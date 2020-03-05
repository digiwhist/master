package eu.datlab.worker.ug.clean;

import eu.datlab.worker.clean.BaseDatlabTenderCleaner;
import eu.dl.dataaccess.dto.codetables.OCDSProcedureMethod;
import eu.dl.dataaccess.dto.codetables.OCDSProcurementCategory;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.codetables.TenderSupplyType;
import eu.dl.worker.clean.plugin.BodyPlugin;
import eu.dl.worker.clean.plugin.DatePlugin;
import eu.dl.worker.clean.plugin.DateTimePlugin;
import eu.dl.worker.clean.plugin.LotPlugin;
import eu.dl.worker.clean.plugin.PricePlugin;
import eu.dl.worker.clean.plugin.PublicationPlugin;
import eu.dl.worker.clean.plugin.TenderProcedureTypePlugin;
import eu.dl.worker.clean.plugin.TenderSupplyTypePlugin;

import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Basic tender cleaner for Uganda.
 *
 * @author Tomas Mrazek
 */
abstract class BaseGPPTenderCleaner extends BaseDatlabTenderCleaner {
    private static final Locale LOCALE = new Locale("en");

    private static final NumberFormat NUMBER_FORMAT = NumberFormat.getInstance(LOCALE);

    private static final List<DateTimeFormatter> DATETIME_FORMATTERS = Arrays.asList(
        DateTimeFormatter.ISO_DATE_TIME, DateTimeFormatter.ofPattern("uuuu-M-d'T'H:m[:s]")
    );

    @SuppressWarnings("unchecked")
    @Override
    protected final void registerSpecificPlugins() {
        pluginRegistry
            .registerPlugin("date", new DatePlugin(DATETIME_FORMATTERS))
            .registerPlugin("datetime", new DateTimePlugin(DATETIME_FORMATTERS))
            .registerPlugin("bodies", new BodyPlugin(null, null))
            .registerPlugin("lots", new LotPlugin(NUMBER_FORMAT, DATETIME_FORMATTERS, new HashMap<>()))
            .registerPlugin("publications", new PublicationPlugin(NUMBER_FORMAT, DATETIME_FORMATTERS, getFormTypeMapping()))
            .registerPlugin("prices", new PricePlugin(NUMBER_FORMAT))
            .registerPlugin("supplyType", new TenderSupplyTypePlugin(getSupplyTypeMapping()))
            .registerPlugin("procedureType", new TenderProcedureTypePlugin(getProcedureTypeMapping()));
    }

    /**
     * @return procedure type mapping
     */
    private static Map<Enum, List<String>> getProcedureTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderProcedureType.OPEN, Arrays.asList(OCDSProcedureMethod.OPEN.name()));
        mapping.put(TenderProcedureType.NEGOTIATED_WITHOUT_PUBLICATION, Arrays.asList(OCDSProcedureMethod.DIRECT.name()));
        mapping.put(TenderProcedureType.APPROACHING_BIDDERS, Arrays.asList(OCDSProcedureMethod.LIMITED.name()));
        mapping.put(TenderProcedureType.RESTRICTED, Arrays.asList(OCDSProcedureMethod.SELECTIVE.name()));

        return mapping;
    }

    /**
     * @return form type mapping
     */
    private Map<Enum, List<String>> getFormTypeMapping() {
        final Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(PublicationFormType.CONTRACT_NOTICE, Arrays.asList("planning", "tender"));
        mapping.put(PublicationFormType.CONTRACT_AWARD, Arrays.asList("award", "contract"));
        mapping.put(PublicationFormType.CONTRACT_IMPLEMENTATION, Arrays.asList("implementation"));

        return mapping;
    }

    /**
     * @return tender supply type mapping
     */
    private Map<Enum, List<String>> getSupplyTypeMapping() {
        Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderSupplyType.WORKS, Arrays.asList(OCDSProcurementCategory.WORKS.name()));
        mapping.put(TenderSupplyType.SUPPLIES, Arrays.asList(OCDSProcurementCategory.GOODS.name()));
        mapping.put(TenderSupplyType.SERVICES, Arrays.asList(OCDSProcurementCategory.SERVICES.name()));

        return mapping;
    }
}
