package eu.datlab.worker.ro.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.raw.RawData;

import java.util.List;

import org.slf4j.Logger;

/**
 * Parser handler for RO source in csv format.
 */
public final class APATenderCsvHandler {

    private static int minHeaderSizeFormOne = 20;
    private static int minHeaderSizeFormTwo = 36;
    private static int minHeaderSizeFormThree = 16;

    /**
     * Private constructor for nonistantiability.
     */
    private APATenderCsvHandler(){

    }

    /**
     * Parses the given raw data.
     * @param raw raw data
     * @param logger logger
     * @return list of parsed tenders
     */
    public static List<ParsedTender> parse(final RawData raw, final Logger logger) {
        final String firstLine = raw.getSourceData().split("\n", 2)[0];
        final int headerNumberOfValues = firstLine.split("[/^]").length;

        if (headerNumberOfValues >= minHeaderSizeFormTwo) {
            return APATenderCsvContracteHandler.parse(raw, logger);
        } else if (headerNumberOfValues >= minHeaderSizeFormOne) {
            return APATenderCsvDirectContractHandler.parse(raw, logger);
        } else {
            return APATenderCsvCFTHandler.parse(raw, logger);
        }
    }
}
