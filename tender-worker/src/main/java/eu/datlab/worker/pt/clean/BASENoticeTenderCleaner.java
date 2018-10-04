package eu.datlab.worker.pt.clean;

import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.BodyPlugin;
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
 * Tender notice cleaner for Portugal.
 *
 * @author Tomas Mrazek
 */
public final class BASENoticeTenderCleaner extends BaseBASECleaner {
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
            .registerPlugin("price", new PricePlugin(NUMBER_FORMAT));
    }

    /**
     * @return publication form type mapping
     */
    private Map<Enum, List<String>> getFormTypeMapping() {
        Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(PublicationFormType.CONTRACT_NOTICE, Arrays.asList("Anúncio de procedimento",
            "Anúncio de concurso urgente"));
        mapping.put(PublicationFormType.CONTRACT_UPDATE, Arrays.asList("Declaração de retificação de anúncio",
            "Aviso de prorrogação de prazo", "Incrementos superiores a 15%"));
        
        return mapping;
    }

    /**
     * @return tender procedure type mapping
     */
    private Map<Enum, List<String>> getProcedureTypeMapping() {
        Map<Enum, List<String>> mapping = new HashMap<>();

        mapping.put(TenderProcedureType.OPEN, Arrays.asList("Concurso público"));
        mapping.put(TenderProcedureType.NEGOTIATED_WITHOUT_PUBLICATION, Arrays.asList("Concurso público urgente"));
        mapping.put(TenderProcedureType.NEGOTIATED, Arrays.asList("Procedimento de negociação"));
        mapping.put(TenderProcedureType.RESTRICTED, Arrays.asList("Concurso limitado por prévia qualificação"));
        mapping.put(TenderProcedureType.COMPETITIVE_DIALOG, Arrays.asList("Diálogo concorrencial"));
        mapping.put(TenderProcedureType.DESIGN_CONTEST, Arrays.asList("Concurso de concepção"));
        mapping.put(TenderProcedureType.APPROACHING_BIDDERS, Arrays.asList("Anúncio simplificado"));
        mapping.put(TenderProcedureType.CONCESSION, Arrays.asList("Instituição de sistema de qualificação",
            "Intenção de celebração de empreitadas de obras publicas por concessionários que não sejam entidades"
                + " adjudicantes"));
        
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
