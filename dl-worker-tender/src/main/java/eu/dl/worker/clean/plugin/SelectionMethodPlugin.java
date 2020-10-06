package eu.dl.worker.clean.plugin;

import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.SelectionMethod;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.utils.SelectionMethodUtils;

/**
 * Plugin used to clean tender selection method.
 *
 * @author Tomas Mrazek
 */
public class SelectionMethodPlugin extends BaseCleaningPlugin<ParsedTender, CleanTender> {

    private final Map<Enum, List<String>> selectionMethodMapping;
    private final Map<Enum, List<Pattern>> selectionMethodRegexMapping;
    private final SelectionMethod defaultValue;

    /**
     * SelectionMethodPlugin should be initialised by the selection method mapping,
     * selectionMethodRegexMapping is initialised with null by default.
     *
     * @param selectionMethodMapping
     *         selection method mapping
     */
    public SelectionMethodPlugin(final Map<Enum, List<String>> selectionMethodMapping) {
        this.selectionMethodMapping = selectionMethodMapping;
        this.selectionMethodRegexMapping = null;
        this.defaultValue = null;
    }

    /**
     * SelectionMethodPlugin and selectionMethodRegexMapping should be initialised by the selection method mapping
     * and selection method regex mapping.
     *
     * @param selectionMethodMapping
     *         selection method mapping
     * @param selectionMethodRegexMapping
     *         selection method regex mapping
     * @param defaultValue
     *         default value
     */
    public SelectionMethodPlugin(final Map<Enum, List<String>> selectionMethodMapping,
                                 final Map<Enum, List<Pattern>> selectionMethodRegexMapping,
                                 final SelectionMethod defaultValue) {
        this.selectionMethodMapping = selectionMethodMapping;
        this.selectionMethodRegexMapping = selectionMethodRegexMapping;
        this.defaultValue = defaultValue;
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
                    selectionMethodMapping, selectionMethodRegexMapping, defaultValue));
            logger.debug("Cleaning selection method for parsed tender {} finished", parsedTender.getId());
        }

        return cleanTender;
    }
}
