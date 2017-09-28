package eu.dl.worker.clean.plugin;

import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.codetables.TenderSupplyType;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.utils.CodeTableUtils;
import java.util.List;
import java.util.Map;

/**
 * Plugins used to clean supply type field.
 *
 * @author Tomas Mrazek
 */
public final class TenderSupplyTypePlugin extends CodeTablePlugin<ParsedTender, CleanTender> {

    /**
     * Tender supply type cleaner with mapping.
     *
     * @param mapping
     *      mapping of the values
     */
    public TenderSupplyTypePlugin(final Map<Enum, List<String>> mapping) {
        super(mapping);
    }

    /**
     * Cleans tenderSupplyType field.
     *
     * @param parsedTender
     *            tender with source data
     * @param cleanTender
     *            tender with clean data
     *
     * @return tender with cleaned data
     */
    @Override
    public CleanTender clean(final ParsedTender parsedTender, final CleanTender cleanTender) {
        final TenderSupplyType supplyType =
            (TenderSupplyType) CodeTableUtils.mapValue(parsedTender.getSupplyType(), mapping);
        cleanTender.setSupplyType(supplyType);
        logger.debug("Cleaned supplyType in parsed tender {}, clean value \"{}\"", parsedTender.getId(), supplyType);

        return cleanTender;
    }
}
