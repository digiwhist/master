package eu.dl.worker.clean.utils;

import eu.dl.dataaccess.dto.codetables.SelectionMethod;

import java.util.List;
import java.util.Map;

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
}
