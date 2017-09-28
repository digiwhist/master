package eu.dl.worker.clean.plugin;

import java.util.List;
import java.util.Map;

import eu.dl.dataaccess.dto.clean.CleanBody;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.utils.BodyUtils;
import eu.dl.worker.utils.ArrayUtils;

/**
 * Plugin used to clean parsed body.
 *
 * @author Tomas Mrazek
 */
public class BodyPlugin extends BaseCleaningPlugin<ParsedTender, CleanTender> {
    /**
     * Body type mapping.
     */
    private Map<Enum, List<String>> typeMapping;
    /**
     * Body main activity mapping.
     */
    private Map<Enum, List<String>> activityMapping;
    /**
     * Body main country address mapping.
     */
    private Map<Enum, List<String>> countryMapping;

    /**
     * Plugin constructor with configuration.
     *
     * @param typeMapping
     *      mapping for body type
     * @param activityMapping
     *      mapping for body main activity
     */
    public BodyPlugin(final Map<Enum, List<String>> typeMapping, final Map<Enum, List<String>> activityMapping) {
        this.typeMapping = typeMapping;
        this.activityMapping = activityMapping;
    }

    /**
     * Plugin constructor with configuration.
     *
     * @param typeMapping
     *      mapping for body type
     * @param activityMapping
     *      mapping for body main activity
     * @param countryMapping
     *      mapping for country address
     */
    public BodyPlugin(final Map<Enum, List<String>> typeMapping, final Map<Enum, List<String>> activityMapping,
                      final Map<Enum, List<String>> countryMapping) {
        this.typeMapping = typeMapping;
        this.activityMapping = activityMapping;
        this.countryMapping = countryMapping;
    }

    /**
     * Cleans body fields.
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
        logger.debug("Cleaning buyers for parsed tender {} starts", parsedTender.getId());
        cleanTender.setBuyers(cleanBody(parsedTender.getBuyers()));
        logger.debug("Cleaning buyers for parsed tender {} finished", parsedTender.getId());

        logger.debug("Cleaning administrators for parsed tender {} starts", parsedTender.getId());
        cleanTender.setAdministrators(cleanBody(parsedTender.getAdministrators()));
        logger.debug("Cleaning administrators for parsed tender {} finished", parsedTender.getId());

        logger.debug("Cleaning approachedBidders for parsed tender {} starts", parsedTender.getId());
        cleanTender.setApproachedBidders(cleanBody(parsedTender.getApproachedBidders()));
        logger.debug("Cleaning approachedBidders for parsed tender {} finished", parsedTender.getId());

        logger.debug("Cleaning candidates for parsed tender {} starts", parsedTender.getId());
        cleanTender.setCandidates(cleanBody(parsedTender.getCandidates()));
        logger.debug("Cleaning candidates for parsed tender {} finished", parsedTender.getId());

        logger.debug("Cleaning supervisors for parsed tender {} starts", parsedTender.getId());
        cleanTender.setSupervisors(cleanBody(parsedTender.getSupervisors()));
        logger.debug("Cleaning supervisors for parsed tender {} finished", parsedTender.getId());

        logger.debug("Cleaning specificationsCreator for parsed tender {} starts", parsedTender.getId());
        cleanTender.setSpecificationsCreator(cleanBody(parsedTender.getSpecificationsCreator()));
        logger.debug("Cleaning specificationsCreator for parsed tender {} finished", parsedTender.getId());

        logger.debug("Cleaning specificationsProvider for parsed tender {} starts", parsedTender.getId());
        cleanTender.setSpecificationsProvider(cleanBody(parsedTender.getSpecificationsProvider()));
        logger.debug("Cleaning specificationsProvider for parsed tender {} finished", parsedTender.getId());

        logger.debug("Cleaning bidsRecipient for parsed tender {} starts", parsedTender.getId());
        cleanTender.setBidsRecipient(cleanBody(parsedTender.getBidsRecipient()));
        logger.debug("Cleaning bidsRecipient for parsed tender {} finished", parsedTender.getId());

        logger.debug("Cleaning furtherInformationProvider for parsed tender {} starts", parsedTender.getId());
        cleanTender.setFurtherInformationProvider(cleanBody(parsedTender.getFurtherInformationProvider()));
        logger.debug("Cleaning furtherInformationProvider for parsed tender {} finished", parsedTender.getId());

        logger.debug("Cleaning onBehalfOf for parsed tender {} starts", parsedTender.getId());
        cleanTender.setOnBehalfOf(cleanBody(parsedTender.getOnBehalfOf()));
        logger.debug("Cleaning onBehalfOf for parsed tender {} finished", parsedTender.getId());

        return cleanTender;
    }

    /**
     * Cleans TED parsedBody.
     *
     * @param parsedBody
     *      parsed body
     * @return clean body
     */
    private CleanBody cleanBody(final ParsedBody parsedBody) {
        if (countryMapping == null) {
            return BodyUtils.cleanBody(parsedBody, typeMapping, activityMapping);
        } else {
            return BodyUtils.cleanBody(parsedBody, typeMapping, activityMapping, countryMapping);
        }

    }

    /**
     * Cleans the list of TED parsedBodies.
     *
     * @param parsedBodies
     *      list of parsed bodies
     * @return list of cleaned bodies
     */
    private List<CleanBody> cleanBody(final List<ParsedBody> parsedBodies) {
        return ArrayUtils.walk(parsedBodies, (parsedBody) -> cleanBody(parsedBody));
    }
}
