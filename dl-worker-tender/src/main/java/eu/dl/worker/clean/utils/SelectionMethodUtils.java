package eu.dl.worker.clean.utils;

import eu.dl.dataaccess.dto.codetables.SelectionMethod;

import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

/**
 * This class provides method for selection method cleaning.
 */
public final class SelectionMethodUtils {

    /**
     * Utility classes should not have default constructor.
     */
    private SelectionMethodUtils() {

    }

    /**
     * Contains logic for mapping parsed selection method to code book value.
     *
     * @param selectionMethod
     *         parsed selection method
     * @param selectionMethodMapping
     *         mapping for selection method
     *
     * @return clean selection method or null if unable to map
     */
    public static SelectionMethod cleanSelectionMethod(final String selectionMethod,
            final Map<Enum, List<String>> selectionMethodMapping) {
        if (selectionMethod != null) {
            return (SelectionMethod) CodeTableUtils.mapValue(selectionMethod, selectionMethodMapping);
        }
        return null;
    }

    /**
     * Contains logic for mapping parsed selection method to code book value with regular expression.
     *
     * @param selectionMethod
     *         parsed selection method
     * @param selectionMethodMapping
     *         mapping for selection method
     * @param selectionMethodRegexMapping
     *         regex mapping for selection method
     * @param defaultValue
     *         default value
     *
     * @return clean selection method or null if unable to map
     */
    public static SelectionMethod cleanSelectionMethod(final String selectionMethod,
                                                       final Map<Enum, List<String>> selectionMethodMapping,
                                                       final Map<Enum, List<Pattern>> selectionMethodRegexMapping,
                                                       final SelectionMethod defaultValue) {
        if (selectionMethod != null) {
            SelectionMethod value = (SelectionMethod) CodeTableUtils.mapValue(selectionMethod, selectionMethodMapping);
            if(value == null && selectionMethodRegexMapping != null) {
                return (SelectionMethod) CodeTableUtils.mapRegex(selectionMethod, selectionMethodRegexMapping, defaultValue);
            }
            return value;
        }
        return null;
    }
}
