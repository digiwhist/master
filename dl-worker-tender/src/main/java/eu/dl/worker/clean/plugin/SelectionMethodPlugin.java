package eu.dl.worker.clean.plugin;

import java.util.List;
import java.util.Map;

import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.utils.SelectionMethodUtils;

/**
 * Plugin used to clean tender selection method.
 *
 * @author Tomas Mrazek
 */
public class SelectionMethodPlugin extends BaseCleaningPlugin<ParsedTender, CleanTender> {

    private final Map<Enum, List<String>> selectionMethodMapping;

    /**
     * SelectionMethodPlugin should be initialised by the selection method mapping.
     *
     * @param selectionMethodMapping
     *         selection method mapping
     */
    public SelectionMethodPlugin(final Map<Enum, List<String>> selectionMethodMapping) {
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
        if (parsedTender.getSelectionMethod() != null) {
            logger.debug("Cleaning selection method for parsed tender {} starts", parsedTender.getId());
            cleanTender.setSelectionMethod(SelectionMethodUtils.cleanSelectionMethod(parsedTender.getSelectionMethod(),
                    selectionMethodMapping));
            logger.debug("Cleaning selection method for parsed tender {} finished", parsedTender.getId());
        }

        return cleanTender;
    }
}
