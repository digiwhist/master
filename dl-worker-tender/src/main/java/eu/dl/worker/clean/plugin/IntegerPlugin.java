package eu.dl.worker.clean.plugin;


import java.text.NumberFormat;
import java.util.List;

import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.utils.NumberUtils;

/**
 * One of generic plugins used to clean basic integer fields.
 *
 * @author Kuba Krafka
 */
public final class IntegerPlugin extends BaseNumberPlugin<ParsedTender, CleanTender> {
    /**
     * Constructor with number format.
     * 
     * @param format
     *            number format
     */
    public IntegerPlugin(final NumberFormat format) {
        super(format);
    }

    /**
     * Constructor with list of number formats.
     * 
     * @param formats
     *            number formats
     */
    public IntegerPlugin(final List<NumberFormat> formats) {
        super(formats);
    }

    /**
     * Cleans integer fields. Those fields get cleaned:
     *  estimatedDurationInMonths
     *  estimatedDurationInDay
     *  maxBidsCount
     *  maxFrameworkAgreementParticipants
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
        final Integer estimatedDurationInDays =
            NumberUtils.cleanInteger(parsedTender.getEstimatedDurationInDays(), formats);
        cleanTender.setEstimatedDurationInDays(estimatedDurationInDays);
        logger.debug("Cleaned estimatedDurationInDays for parsed tender {}, clean value \"{}\"",
            parsedTender.getId(), estimatedDurationInDays);

        final Integer estimatedDurationInMonths =
            NumberUtils.cleanInteger(parsedTender.getEstimatedDurationInMonths(), formats);
        cleanTender.setEstimatedDurationInMonths(estimatedDurationInMonths);
        logger.debug("Cleaned estimatedDurationInMonths for parsed tender {}, clean value \"{}\"",
            parsedTender.getId(), estimatedDurationInMonths);

        final Integer estimatedDurationInYears =
                NumberUtils.cleanInteger(parsedTender.getEstimatedDurationInYears(), formats);
        cleanTender.setEstimatedDurationInYears(estimatedDurationInYears);
        logger.debug("Cleaned estimatedDurationInYears for parsed tender {}, clean value \"{}\"",
                parsedTender.getId(), estimatedDurationInYears);

        final Integer maxFrameworkAgreementParticipants =
            NumberUtils.cleanInteger(parsedTender.getMaxFrameworkAgreementParticipants(), formats);
        cleanTender.setMaxFrameworkAgreementParticipants(maxFrameworkAgreementParticipants);
        logger.debug("Cleaned maxFrameworkAgreementParticipants for parsed tender {}, clean value \"{}\"",
            parsedTender.getId(), maxFrameworkAgreementParticipants);

        final Integer maxBidsCount = NumberUtils.cleanInteger(parsedTender.getMaxBidsCount(), formats);
        cleanTender.setMaxBidsCount(maxBidsCount);
        logger.debug(
            "Cleaned maxBidsCount for parsed tender {}, clean value \"{}\"", parsedTender.getId(), maxBidsCount);

        final Integer awardDeadlineDuration =
                NumberUtils.cleanInteger(parsedTender.getAwardDeadlineDuration(), formats);
        cleanTender.setAwardDeadlineDuration(awardDeadlineDuration);
        logger.debug("Cleaned awardDeadlineDuration for parsed tender {}, clean value \"{}\"",
                parsedTender.getId(), awardDeadlineDuration);

        final Integer envisagedCandidatesCount =
                NumberUtils.cleanInteger(parsedTender.getEnvisagedCandidatesCount(), formats);
        cleanTender.setEnvisagedCandidatesCount(envisagedCandidatesCount);
        logger.debug("Cleaned envisagedCandidatesCount for parsed tender {}, clean value \"{}\"",
                parsedTender.getId(), envisagedCandidatesCount);

        final Integer envisagedMinCandidatesCount =
                NumberUtils.cleanInteger(parsedTender.getEnvisagedMinCandidatesCount(), formats);
        cleanTender.setEnvisagedMinCandidatesCount(envisagedMinCandidatesCount);
        logger.debug("Cleaned envisagedMinCandidatesCount for parsed tender {}, clean value \"{}\"",
                parsedTender.getId(), envisagedMinCandidatesCount);

        final Integer envisagedMaxCandidatesCount =
                NumberUtils.cleanInteger(parsedTender.getEnvisagedMaxCandidatesCount(), formats);
        cleanTender.setEnvisagedMaxCandidatesCount(envisagedMaxCandidatesCount);
        logger.debug("Cleaned envisagedMaxCandidatesCount for parsed tender {}, clean value \"{}\"",
                parsedTender.getId(), envisagedMaxCandidatesCount);

        return cleanTender;
    }
}
