package eu.digiwhist.worker.cz.clean.utils;

import eu.dl.dataaccess.dto.clean.CleanTenderLot;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.clean.utils.LotUtils;

import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * This class provide method for CPV cleaning.
 *
 * @author Tomas Mrazek
 */
public final class VestnikLotUtils {

    /**
     * Utility classes should not have default constructor.
     */
    private VestnikLotUtils() {

    }

    /**
     * Cleans the given lot.
     *
     * @param parsedLot
     *         parsed lot
     * @param numberFormat
     *         list of number formats
     * @param formatter
     *         datetime formatter
     * @param documentTypeMapping
     *         mapping for document type
     * @param selectionMethodMapping
     *         mapping for selection method
     * @param statusMapping
     *         mapping for status
     * @param unitPriceMapping
     *         mapping for price units
     *
     * @return cleaned lot
     */
    public static CleanTenderLot cleanLot(final ParsedTenderLot parsedLot, final List<NumberFormat> numberFormat,
            final List<DateTimeFormatter> formatter, final Map<Enum, List<String>> documentTypeMapping,
            final Map<Enum, List<String>> selectionMethodMapping, final Map<Enum, List<String>> statusMapping,
            final Map<Enum, List<String>> unitPriceMapping) {
        if (parsedLot == null) {
            return null;
        }

        final Map<String, Map<Enum, List<String>>> lotMapping = new HashMap<>();
        lotMapping.put("documentTypeMapping", documentTypeMapping);
        lotMapping.put("selectionMethodMapping", selectionMethodMapping);
        lotMapping.put("statusMapping", statusMapping);
        lotMapping.put("unitPriceMapping", unitPriceMapping);


        CleanTenderLot cleanLot = LotUtils.cleanLot(parsedLot, numberFormat, formatter, lotMapping);

        // Vestnik specific selection method cleaning
        cleanLot.setSelectionMethod(
                VestnikSelectionMethodUtils.cleanSelectionMethod(parsedLot.getSelectionMethod(), selectionMethodMapping,
                        parsedLot.getAwardCriteria()));

        return cleanLot;
    }
}
