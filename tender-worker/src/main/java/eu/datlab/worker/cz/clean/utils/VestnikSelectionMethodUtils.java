package eu.datlab.worker.cz.clean.utils;

import eu.dl.dataaccess.dto.codetables.SelectionMethod;
import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.worker.clean.utils.SelectionMethodUtils;

import java.util.List;
import java.util.Map;

/**
 * This class provides method for selection method cleaning.
 */
public final class VestnikSelectionMethodUtils {

    /**
     * Utility classes should not have default constructor.
     */
    private VestnikSelectionMethodUtils() {

    }

    /**
     * Contains logic for mapping parsed selection method to code book value.
     *
     * @param selectionMethod
     *         parsed selection method
     * @param selectionMethodMapping
     *         mapping for selection method
     * @param awardCriteria
     *         parsed award criteria
     *
     * @return clean selection method or null if unable to map
     */
    public static SelectionMethod cleanSelectionMethod(final String selectionMethod,
            final Map<Enum, List<String>> selectionMethodMapping, final List<ParsedAwardCriterion> awardCriteria) {
        // if there is only one price criterion, set selection method to LOWEST_PRICE
        if (awardCriteria != null && awardCriteria.size() == 1 && awardCriteria.get(0)
                .getName()
                .equalsIgnoreCase("price")) {
            return SelectionMethod.LOWEST_PRICE;
        }
        // otherwise use the common selection method plugin and map source selection method value
        return SelectionMethodUtils.cleanSelectionMethod(selectionMethod, selectionMethodMapping);
    }
}
