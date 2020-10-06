package eu.datlab.worker.ug.clean;

import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.parsed.ParsedTender;

/**
 * Tender cleaner for Uganda.
 *
 * @author Tomas Mrazek
 */
public class GPPTenderCleaner extends BaseGPPTenderCleaner {
    private static final String VERSION = "1.0";

    @Override
    public final String getVersion() {
        return VERSION;
    }

    @Override
    protected final ParsedTender preProcessParsedItem(final ParsedTender parsedItem) {
        return parsedItem;
    }

    @Override
    protected final CleanTender postProcessSourceSpecificRules(final ParsedTender parsedTender, final CleanTender cleanTender) {
        return cleanTender;
    }
}
