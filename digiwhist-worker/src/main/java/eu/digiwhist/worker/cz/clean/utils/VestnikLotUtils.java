package eu.digiwhist.worker.cz.clean.utils;

import eu.dl.dataaccess.dto.clean.CleanTenderLot;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.clean.utils.LotUtils;

import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
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
     * @param mappings
     *         mappings for {@link LotUtils#cleanLot(eu.dl.dataaccess.dto.parsed.ParsedTenderLot, java.util.List,
     *          java.util.List, java.util.Map, java.lang.String)}     
     * @param country
     *          country
     * @return cleaned lot
     */
    public static CleanTenderLot cleanLot(final ParsedTenderLot parsedLot, final List<NumberFormat> numberFormat,
        final List<DateTimeFormatter> formatter, final Map<String, Map<Enum, List<String>>> mappings,
        final String country) {

        if (parsedLot == null) {
            return null;
        }

        CleanTenderLot cleanLot = LotUtils.cleanLot(parsedLot, numberFormat, formatter, mappings, country);

        // Vestnik specific selection method cleaning
        cleanLot.setSelectionMethod(
                VestnikSelectionMethodUtils.cleanSelectionMethod(parsedLot.getSelectionMethod(),
                    mappings.get("selectionMethodMapping"), parsedLot.getAwardCriteria()));

        return cleanLot;
    }
}
