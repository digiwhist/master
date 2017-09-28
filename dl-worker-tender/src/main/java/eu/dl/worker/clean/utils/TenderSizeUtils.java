package eu.dl.worker.clean.utils;

import eu.dl.dataaccess.dto.codetables.TenderSize;
import java.util.List;
import java.util.Map;

/**
 * This class provides method for tender size cleaning.
 */
public final class TenderSizeUtils {

    /**
     * Utility classes should not have default constructor.
     */
    private TenderSizeUtils() {

    }

    /**
     * Contains logic for mapping parsed tender size to code book value.
     *
     * @param size
     *         parsed tender size
     * @param sizeMapping
     *         mapping for tender size
     *
     * @return clean size or null if unable to map
     */
    public static TenderSize cleanSize(final String size, final Map<Enum, List<String>> sizeMapping) {
        if (size != null) {
            return (TenderSize) CodeTableUtils.mapValue(size, sizeMapping);
        }
        return null;
    }
}
