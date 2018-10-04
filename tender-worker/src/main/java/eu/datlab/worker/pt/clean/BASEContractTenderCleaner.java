package eu.datlab.worker.pt.clean;

import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.AddressPlugin;
import eu.dl.worker.clean.plugin.BodyPlugin;
import eu.dl.worker.clean.plugin.DatePlugin;
import eu.dl.worker.clean.plugin.DocumentPlugin;
import eu.dl.worker.clean.plugin.IntegerPlugin;
import eu.dl.worker.clean.plugin.LotPlugin;
import eu.dl.worker.clean.plugin.PricePlugin;
import eu.dl.worker.clean.plugin.PublicationPlugin;
import eu.dl.worker.clean.plugin.TenderProcedureTypePlugin;
import eu.dl.worker.clean.plugin.TenderSupplyTypePlugin;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Tender contract cleaner for Portugal.
 *
 * @author Tomas Mrazek
 */
public final class BASEContractTenderCleaner extends BaseBASECleaner {
    private static final String VERSION = "1.0";

    @Override
    protected void registerSpecificPlugins() {
        pluginRegistry
            .registerPlugin("integer", new IntegerPlugin(NUMBER_FORMAT))
            .registerPlugin("publications", new PublicationPlugin(NUMBER_FORMAT, DATE_FORMATTER, getFormTypeMapping()))
            .registerPlugin("supplyType", new TenderSupplyTypePlugin(getSupplyTypeMapping()))
            .registerPlugin("procedureType", new TenderProcedureTypePlugin(getProcedureTypeMapping()))
            .registerPlugin("lot", new LotPlugin(NUMBER_FORMAT, DATE_FORMATTER, new HashMap<>()))
            .registerPlugin("body", new BodyPlugin(null, null))
            .registerPlugin("address", new AddressPlugin())
            .registerPlugin("date", new DatePlugin(DATE_FORMATTER))
            .registerPlugin("price", new PricePlugin(NUMBER_FORMAT))
            .registerPlugin("documents", new DocumentPlugin(NUMBER_FORMAT, DATE_FORMATTER, null));

    }

    /**
     * @return publication form type mapping
     */
    private Map<Enum, List<String>> getFormTypeMapping() {
        Map<Enum, List<String>> mapping = new HashMap<>();
        
        mapping.put(PublicationFormType.CONTRACT_AWARD, Arrays.asList("CONTRACT_AWARD"));

        return mapping;
    }

    /**
     * @return tender procedure type mapping
     */
    private Map<Enum, List<String>> getProcedureTypeMapping() {
        Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderProcedureType.OPEN, Arrays.asList("Concurso público"));
        mapping.put(TenderProcedureType.OUTRIGHT_AWARD, Arrays.asList("Ajuste direto"));
        mapping.put(TenderProcedureType.RESTRICTED, Arrays.asList("Concurso limitado por prévia qualificação"));
        mapping.put(TenderProcedureType.NEGOTIATED, Arrays.asList("Procedimento de negociação"));
        mapping.put(TenderProcedureType.COMPETITIVE_DIALOG, Arrays.asList("Diálogo concorrencial"));
        mapping.put(TenderProcedureType.MINITENDER, Arrays.asList("Ao abrigo do acordo-quadro (art.º 258.º)",
            "Ao abrigo do acordo-quadro (art.º 259.º)"));
        
        return mapping;
    }

    @Override
    protected CleanTender postProcessSourceSpecificRules(final ParsedTender parsedItem, final CleanTender cleanItem) {
        return cleanItem;
    }

    @Override
    protected ParsedTender preProcessParsedItem(final ParsedTender parsedItem) {
        return parsedItem;
    }

    @Override
    public String getVersion() {
        return VERSION;
    }
}
