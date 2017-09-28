package eu.dl.worker.clean.plugin;

import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.utils.StringUtils;

/**
 * Plugin cleaning tenders long text fields such .
 *
 * @author Kuba Krafka
 */
public final class LongTextPlugin extends BaseCleaningPlugin<ParsedTender, CleanTender> {

    /**
     * Cleans the long text fields defined in the list bellow. description
     * descriptionEnglish personalRequirements economicRequirements
     * technicalRequirements nationalProcedureType buyerAssignedId deposits
     * appealBodyName mediationBodyName cancellationReason npwpReasons modificationReason
     * modificationReasonDescription
     *
     * @param parsedTender
     *         tender with source data
     * @param cleanTender
     *         tender with clean data
     *
     * @return cleaned tender
     */
    @Override
    public CleanTender clean(final ParsedTender parsedTender, final CleanTender cleanTender) {
        final String cleanedDescription = StringUtils.cleanLongString(parsedTender.getDescription());
        cleanTender.setDescription(cleanedDescription);
        logger.debug("Cleaned Description in parsed tender {}, clean value \"{}\"", parsedTender.getId(),
                cleanedDescription);

        final String cleanedDescriptionEnglish = StringUtils.cleanLongString(parsedTender.getDescriptionEnglish());
        cleanTender.setDescriptionEnglish(cleanedDescriptionEnglish);
        logger.debug("Cleaned DescriptionEnglish in parsed tender {}, clean value \"{}\"", parsedTender.getId(),
                cleanedDescriptionEnglish);

        final String cleanedPersonalRequirements = StringUtils.cleanLongString(parsedTender.getPersonalRequirements());
        cleanTender.setPersonalRequirements(cleanedPersonalRequirements);
        logger.debug("Cleaned PersonalRequirements in parsed tender {}, clean value \"{}\"", parsedTender.getId(),
                cleanedPersonalRequirements);

        final String cleanedEconomicRequirements = StringUtils.cleanLongString(parsedTender.getEconomicRequirements());
        cleanTender.setEconomicRequirements(cleanedEconomicRequirements);
        logger.debug("Cleaned EconomicRequirements in parsed tender {}, clean value \"{}\"", parsedTender.getId(),
                cleanedEconomicRequirements);

        final String cleanedTechnicalRequirements = StringUtils.cleanLongString(
                parsedTender.getTechnicalRequirements());
        cleanTender.setTechnicalRequirements(cleanedTechnicalRequirements);
        logger.debug("Cleaned TechnicalRequirements in parsed tender {}, clean value \"{}\"", parsedTender.getId(),
                cleanedTechnicalRequirements);

        final String cleanedNationalProcedureType = StringUtils.cleanLongString(
                parsedTender.getNationalProcedureType());
        cleanTender.setNationalProcedureType(cleanedNationalProcedureType);
        logger.debug("Cleaned NationalProcedureType in parsed tender {}, clean value \"{}\"", parsedTender.getId(),
                cleanedNationalProcedureType);

        final String cleanedDeposits = StringUtils.cleanLongString(parsedTender.getDeposits());
        cleanTender.setDeposits(cleanedDeposits);
        logger.debug("Cleaned Deposits in parsed {}, clean value \"{}\"", parsedTender.getId(), cleanedDeposits);

        final String cleanedAppealBodyName = StringUtils.cleanLongString(parsedTender.getAppealBodyName());
        cleanTender.setAppealBodyName(cleanedAppealBodyName);
        logger.debug("Cleaned AppealBodyName in parsed tender {}, clean value \"{}\"", parsedTender.getId(),
                cleanedAppealBodyName);

        final String cleanedMediationBodyName = StringUtils.cleanLongString(parsedTender.getMediationBodyName());
        cleanTender.setMediationBodyName(cleanedMediationBodyName);
        logger.debug("Cleaned MediationBodyName in parsed tender {}, clean value \"{}\"", parsedTender.getId(),
                cleanedMediationBodyName);

        final String cleanedCancellationReason = StringUtils.cleanLongString(parsedTender.getCancellationReason());
        cleanTender.setCancellationReason(cleanedCancellationReason);
        logger.debug("Cleaned CancellationReason in parsed tender {}, clean value \"{}\"", parsedTender.getId(),
                cleanedCancellationReason);

        final String modificationReason = StringUtils.cleanLongString(parsedTender.getModificationReason());
        cleanTender.setModificationReason(modificationReason);
        logger.debug("Cleaned ModificationReason in parsed tender {}, clean value \"{}\"", parsedTender.getId(),
                modificationReason);

        final String modificationReasonDescription = StringUtils.cleanLongString(
                parsedTender.getModificationReasonDescription());
        cleanTender.setModificationReasonDescription(modificationReasonDescription);
        logger.debug("Cleaned ModificationReasonDescription in parsed tender {}, clean value \"{}\"",
                parsedTender.getId(), modificationReasonDescription);

        final String eligibilityCriteria = StringUtils.cleanLongString(parsedTender.getEligibilityCriteria());
        cleanTender.setEligibilityCriteria(eligibilityCriteria);
        logger.debug("Cleaned EligibilityCriteria in parsed tender {}, clean value \"{}\"", parsedTender.getId(),
                eligibilityCriteria);

        final String additionalInfo = StringUtils.cleanLongString(parsedTender.getAdditionalInfo());
        cleanTender.setAdditionalInfo(additionalInfo);
        logger.debug("Cleaned AdditionalInfo in parsed tender {}, clean value \"{}\"", parsedTender.getId(),
                additionalInfo);

        return cleanTender;
    }
}
