package eu.dl.worker.clean.plugin;

import java.util.List;
import java.util.Map;

import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.utils.TenderSizeUtils;

/**
 * Plugin used to clean tender size.
 *
 * @author Tomas Mrazek
 */
public class TenderSizePlugin extends BaseCleaningPlugin<ParsedTender, CleanTender> {

    private final Map<Enum, List<String>> tenderSizeMapping;

    /**
     * SelectionMethodPlugin should be initialized by the tender size mapping.
     *
     * @param tenderSizeMapping
     *         tender size mapping
     */
    public TenderSizePlugin(final Map<Enum, List<String>> tenderSizeMapping) {
        this.tenderSizeMapping = tenderSizeMapping;
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
        logger.debug("Cleaning size for parsed tender {} starts", parsedTender.getId());
        cleanTender.setSize(TenderSizeUtils.cleanSize(parsedTender.getSize(), tenderSizeMapping));
        logger.debug("Cleaning size for parsed tender {} finished", parsedTender.getId());
        
        return cleanTender;
    }
}
