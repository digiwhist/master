package eu.dl.worker.clean.plugin;

import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.utils.CPVUtils;

/**
 * Plugin used to clean CPVs codes.
 *
 * @author Tomas Mrazek
 */
public class CpvPlugin extends BaseCleaningPlugin<ParsedTender, CleanTender> {

    /**
     * Cleans CPVs.
     *
     * @param parsedTender
     *            tender with source data
     * @param cleanTender
     *            tender with clean data
     *
     * @return tender with cleaned data
     */
    @Override
    public final CleanTender clean(final ParsedTender parsedTender, final CleanTender cleanTender) {
        if (parsedTender.getCpvs() != null) {
            logger.debug("Cleaning cpvs for parsed tender {} starts", parsedTender.getId());
            cleanTender.setCpvs(CPVUtils.cleanCpvs(parsedTender.getCpvs()));
            logger.debug("Cleaning cpvs for parsed tender {} finished", parsedTender.getId());
        }

        return cleanTender;
    }
}
