package eu.datlab.worker.sk.clean;

import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.BaseCleaningPlugin;
import eu.dl.worker.clean.plugin.TenderProcedureTypePlugin;
import java.util.List;
import java.util.Map;

/**
 * Tender clean procedureType plugin specific for SK Uvo.
 *
 * @author Michal Riha
 */
public class UvoTenderProcedureTypePlugin extends BaseCleaningPlugin<ParsedTender, CleanTender> {
    private final List<String> accelerated;
    private final Map<Enum, List<String>> mapping;

    /**
     * Tender procedure type cleaner with mapping. In case that {@code accelerated} list contains cleaned value sets
     * cleanTender.isAccelerated to TRUE.
     *
     * @param mapping
     *      mapping of the values
     * @param accelerated
     *      list of parsed procedure types that are accelerated
     */
    public UvoTenderProcedureTypePlugin(final Map<Enum, List<String>> mapping, final List<String> accelerated) {
        this.mapping = mapping;
        this.accelerated = accelerated;
    }

    /**
     * Tender procedure type cleaner with mapping.
     *
     * @param mapping
     *      mapping of the values
     */
    public UvoTenderProcedureTypePlugin(final Map<Enum, List<String>> mapping) {
        this.mapping = mapping;
        this.accelerated = null;
    }

    @Override
    public final CleanTender clean(final ParsedTender parsedTender, final CleanTender cleanTender) {
        return new TenderProcedureTypePlugin(mapping, accelerated).clean(parsedTender, cleanTender);
    }
}
