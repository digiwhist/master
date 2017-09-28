package eu.dl.worker.clean.plugin;

import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.utils.ArrayUtils;
import eu.dl.worker.clean.utils.StringUtils;
import java.util.List;

/**
 * Plugin cleaning short texts such a title/titleEnglish.
 *
 * @author Kuba Krafka
 *
 */
public final class ShortTextPlugin extends BaseCleaningPlugin<ParsedTender, CleanTender> {

    /**
     * Cleans the short text field. Cleaned values are: title titleEnglish
     * buyerAssignedId eligibleBidLanguages
     *
     * @param parsedTender
     *            tender with source data
     * @param cleanTender
     *            tender with clean data
     *
     * @return clean tender
     */
    @Override
    public CleanTender clean(final ParsedTender parsedTender, final CleanTender cleanTender) {
        String cleanedTitle = StringUtils.upperCaseFirst(StringUtils.cleanShortString(parsedTender.getTitle()));
        cleanTender.setTitle(cleanedTitle);
        logger.debug("Cleaned title in parsed tender {}, clean value \"{}\"", parsedTender.getId(), cleanedTitle);

        String cleanedTitleEnglish = StringUtils
                .upperCaseFirst(StringUtils.cleanShortString(parsedTender.getTitleEnglish()));
        cleanTender.setTitleEnglish(cleanedTitleEnglish);
        logger.debug("Cleaned titleEnglish in parsed tender {}, clean value \"{}\"", parsedTender.getId(),
                cleanedTitleEnglish);

        String cleanedBuyerAssignedId = StringUtils.cleanShortString(parsedTender.getBuyerAssignedId());
        cleanTender.setBuyerAssignedId(cleanedBuyerAssignedId);
        logger.debug("Cleaned BuyerAssignedId in parsed tender {}, clean value \"{}\"", parsedTender.getId(),
                cleanedBuyerAssignedId);

        if (parsedTender.getEligibleBidLanguages() != null) {
            final List<String> cleanedEligibleBidLanguages =
                ArrayUtils.walk(parsedTender.getEligibleBidLanguages(), StringUtils::cleanShortString);

            cleanTender.setEligibleBidLanguages(cleanedEligibleBidLanguages);

            logger.debug("Cleaned eligibleBidLanguages in parsed tender {}, clean value \"{}\"", parsedTender.getId(),
                    cleanedEligibleBidLanguages);
        }

        String excessiveFramework = StringUtils.cleanShortString(
            parsedTender.getExcessiveFrameworkAgreementJustification());
        cleanTender.setExcessiveFrameworkAgreementJustification(excessiveFramework);
        logger.debug("Cleaned excessiveFrameworkAgreementJustification in parsed tender {}, clean value \"{}\"",
            parsedTender.getId(), excessiveFramework);

        String nationalProcedureType = StringUtils.cleanShortString(parsedTender.getNationalProcedureType());
        cleanTender.setNationalProcedureType(nationalProcedureType);
        logger.debug("Cleaned nationalProcedureType in parsed tender {}, clean value \"{}\"", parsedTender.getId(),
                nationalProcedureType);

        String acceleratedProcedureJustification = StringUtils.cleanShortString(
                parsedTender.getAcceleratedProcedureJustification());
        cleanTender.setAcceleratedProcedureJustification(acceleratedProcedureJustification);
        logger.debug("Cleaned acceleratedProcedureJustification in parsed tender {}, clean value \"{}\"",
                parsedTender.getId(), acceleratedProcedureJustification);

        return cleanTender;
    }
}
