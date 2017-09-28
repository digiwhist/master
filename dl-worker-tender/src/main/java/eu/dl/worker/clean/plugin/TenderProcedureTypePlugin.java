package eu.dl.worker.clean.plugin;

import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.utils.CodeTableUtils;
import java.util.List;
import java.util.Map;

/**
 * Plugins used to clean procedure type field.
 *
 * @author Tomas Mrazek
 */
public final class TenderProcedureTypePlugin extends CodeTablePlugin<ParsedTender, CleanTender> {
    private final List<String> accelerated;

    /**
     * Tender procedure type cleaner with mapping. In case that {@code accelerated} list contains cleaned value sets
     * cleanTender.isAccelerated to TRUE.
     *
     * @param mapping
     *      mapping of the values
     * @param accelerated
     *      list of parsed procedure types that are accelerated
     */
    public TenderProcedureTypePlugin(final Map<Enum, List<String>> mapping, final List<String> accelerated) {
        super(mapping);
        this.accelerated = accelerated;
    }

    /**
     * Tender procedure type cleaner with mapping.
     *
     * @param mapping
     *      mapping of the values
     */
    public TenderProcedureTypePlugin(final Map<Enum, List<String>> mapping) {
        super(mapping);
        this.accelerated = null;
    }

    /**
     * Cleans tender procedure type field.
     *
     * @param parsedTender
     *            tender with source data
     * @param cleanTender
     *            tender with clean data
     *
     * @return tender with cleaned data
     */
    @Override
    public CleanTender clean(final ParsedTender parsedTender, final CleanTender cleanTender) {
        final String parsedProcedureType = parsedTender.getProcedureType();
        final TenderProcedureType procedureType =
            (TenderProcedureType) CodeTableUtils.mapValue(parsedProcedureType, mapping);
        cleanTender.setProcedureType(procedureType);

        if (parsedProcedureType != null && accelerated != null) {
            for (String value : accelerated) {
                if (parsedProcedureType.equalsIgnoreCase(value)) {
                    cleanTender.setIsAcceleratedProcedure(Boolean.TRUE);
                    break;
                }
            }
        }

        logger.debug("Cleaned procedureType in parsed tender {}, clean value \"{}\"", parsedTender.getId(),
                procedureType);

        return cleanTender;
    }
}
