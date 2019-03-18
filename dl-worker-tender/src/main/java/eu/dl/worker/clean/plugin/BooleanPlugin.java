package eu.dl.worker.clean.plugin;

import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.utils.StringUtils;

/**
 * One of generci plugins used to clean basic boolean fields.
 *
 * @author Tomas Mrazek
 */
public class BooleanPlugin extends BaseCleaningPlugin<ParsedTender, CleanTender> {

    /**
     * Cleans boolean fields.
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
        final Boolean hasLots = StringUtils.cleanBoolean(parsedTender.getHasLots());
        cleanTender.setHasLots(hasLots);
        logger.debug("Cleaned hasLots for parsed tender {}, clean value \"{}\"", parsedTender.getId(), hasLots);

        final Boolean hasOptions = StringUtils.cleanBoolean(parsedTender.getHasOptions());
        cleanTender.setHasOptions(hasOptions);
        logger.debug("Cleaned hasOptions for parsed tender {}, clean value \"{}\"", parsedTender.getId(), hasOptions);

        final Boolean areVariantsAccepted = StringUtils.cleanBoolean(parsedTender.getAreVariantsAccepted());
        cleanTender.setAreVariantsAccepted(areVariantsAccepted);
        logger.debug("Cleaned areVariantsAccepted for parsed tender {}, clean value \"{}\"", parsedTender.getId(),
            areVariantsAccepted);

        final Boolean isCentralProcurement = StringUtils.cleanBoolean(parsedTender.getIsCentralProcurement());
        cleanTender.setIsCentralProcurement(isCentralProcurement);
        logger.debug("Cleaned isCentralProcurement for parsed tender {}, clean value \"{}\"", parsedTender.getId(),
            isCentralProcurement);

        final Boolean isCoveredByGpa = StringUtils.cleanBoolean(parsedTender.getIsCoveredByGpa());
        cleanTender.setIsCoveredByGpa(isCoveredByGpa);
        logger.debug("Cleaned isCoveredByGpa for parsed tender {}, clean value \"{}\"", parsedTender.getId(),
            isCoveredByGpa);

        final Boolean isDps = StringUtils.cleanBoolean(parsedTender.getIsDps());
        cleanTender.setIsDps(isDps);
        logger.debug("Cleaned isDps for parsed tender {}, clean value \"{}\"", parsedTender.getId(), isDps);

        final Boolean isEInvoiceAccepted = StringUtils.cleanBoolean(parsedTender.getIsEInvoiceAccepted());
        cleanTender.setIsEInvoiceAccepted(isEInvoiceAccepted);
        logger.debug("Cleaned isEInvoiceAccepted for parsed tender {}, clean value \"{}\"", parsedTender.getId(),
            isEInvoiceAccepted);

        final Boolean isElectronicAuction = StringUtils.cleanBoolean(parsedTender.getIsElectronicAuction());
        cleanTender.setIsElectronicAuction(isElectronicAuction);
        logger.debug("Cleaned isElectronicAuction for parsed tender {}, clean value \"{}\"", parsedTender.getId(),
            isElectronicAuction);

        final Boolean isFrameworkAgreement = StringUtils.cleanBoolean(parsedTender.getIsFrameworkAgreement());
        cleanTender.setIsFrameworkAgreement(isFrameworkAgreement);
        logger.debug("Cleaned isFrameworkAgreement for parsed tender {}, clean value \"{}\"", parsedTender.getId(),
            isFrameworkAgreement);

        final Boolean isJointProcurement = StringUtils.cleanBoolean(parsedTender.getIsJointProcurement());
        cleanTender.setIsJointProcurement(isJointProcurement);
        logger.debug("Cleaned isJointProcurement for parsed tender {}, clean value \"{}\"", parsedTender.getId(),
            isJointProcurement);

        final Boolean isOnBehalfOf = StringUtils.cleanBoolean(parsedTender.getIsOnBehalfOf());
        cleanTender.setIsOnBehalfOf(isOnBehalfOf);
        logger.debug(
            "Cleaned isOnBehalfOf for parsed tender {}, clean value \"{}\"", parsedTender.getId(), isOnBehalfOf);

        final Boolean isWholeTenderCancelled = StringUtils.cleanBoolean(parsedTender.getIsWholeTenderCancelled());
        cleanTender.setIsWholeTenderCancelled(isWholeTenderCancelled);
        logger.debug("Cleaned isWholeTenderCancelled for parsed tender {}, clean value \"{}\"", parsedTender.getId(),
            isWholeTenderCancelled);

        final Boolean documentsPayable = StringUtils.cleanBoolean(parsedTender.getDocumentsPayable());
        cleanTender.setDocumentsPayable(documentsPayable);
        logger.debug("Cleaned documentsPayable for parsed tender {}, clean value \"{}\"", parsedTender.getId(),
            documentsPayable);

        final Boolean isDocumentsAccessRestricted = StringUtils.cleanBoolean(
                parsedTender.getIsDocumentsAccessRestricted());
        cleanTender.setIsDocumentsAccessRestricted(isDocumentsAccessRestricted);
        logger.debug("Cleaned isDocumentsAccessRestricted for parsed tender {}, clean value \"{}\"",
                parsedTender.getId(), isDocumentsAccessRestricted);

        final Boolean isAwarded = StringUtils.cleanBoolean(
                parsedTender.getIsAwarded());
        cleanTender.setIsAwarded(isAwarded);
        logger.debug("Cleaned isAwarded for parsed tender {}, clean value \"{}\"",
                parsedTender.getId(), isAwarded);

        return cleanTender;
    }
}
