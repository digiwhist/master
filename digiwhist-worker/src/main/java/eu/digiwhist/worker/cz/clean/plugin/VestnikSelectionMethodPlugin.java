package eu.digiwhist.worker.cz.clean.plugin;

import java.util.List;
import java.util.Map;

import eu.digiwhist.worker.cz.clean.utils.VestnikSelectionMethodUtils;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.BaseCleaningPlugin;

/**
 * Plugin used to clean tender selection method.
 */
public class VestnikSelectionMethodPlugin extends BaseCleaningPlugin<ParsedTender, CleanTender> {

    private final Map<Enum, List<String>> selectionMethodMapping;

    /**
     * VestnikSelectionMethodPlugin should be initialised by the selection method mapping.
     *
     * @param selectionMethodMapping
     *         selection method mapping
     */
    public VestnikSelectionMethodPlugin(final Map<Enum, List<String>> selectionMethodMapping) {
        this.selectionMethodMapping = selectionMethodMapping;
    }

    /**
     * Cleans award criteria.
     *
     * @param parsedTender
     *         tender with source data
     * @param cleanTender
     *         tender with clean data
     *
     * @return tender with cleaned data
     */
    @Override
    public final CleanTender clean(final ParsedTender parsedTender, final CleanTender cleanTender) {
        logger.debug("Cleaning selection method for parsed tender {} starts", parsedTender.getId());
        cleanTender.setSelectionMethod(
                VestnikSelectionMethodUtils.cleanSelectionMethod(parsedTender.getSelectionMethod(),
                        selectionMethodMapping, parsedTender.getAwardCriteria()));
        logger.debug("Cleaning selection method for parsed tender {} finished", parsedTender.getId());
        return cleanTender;
    }
}
