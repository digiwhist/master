package eu.dl.worker.clean.utils;

import eu.dl.worker.utils.ArrayUtils;
import java.util.List;
import java.util.Map;

import eu.dl.dataaccess.dto.clean.CleanBody;
import eu.dl.dataaccess.dto.codetables.BuyerActivityType;
import eu.dl.dataaccess.dto.codetables.BuyerType;
import eu.dl.dataaccess.dto.parsed.ParsedBody;

/**
 * This class provide method for body cleaning.
 *
 * @author Tomas Mrazek
 */
public final class BodyUtils {

    /**
     * Utility classes should not have default constructor.
     */
    private BodyUtils() {

    }

    /**
     * Cleans provided parsed body.
     *
     * @param parsedBody
     *          parsed body
     * @param buyerTypeMapping
     *          mapping for buyer type
     * @param buyerActivityTypeMapping
     *          mapping for buyer activity
     * @return cleaned body
     */
    public static CleanBody cleanBody(final ParsedBody parsedBody,
                                      final Map<Enum, List<String>> buyerTypeMapping,
                                      final Map<Enum, List<String>> buyerActivityTypeMapping) {
        if (parsedBody == null) {
            return null;
        }

        return cleanBodyWithoutAddress(parsedBody, buyerTypeMapping, buyerActivityTypeMapping)
                .setAddress(AddressUtils.cleanAddress(parsedBody.getAddress()));
    }

    /**
     * Cleans provided parsed body.
     *
     * @param parsedBody
     *          parsed body
     * @param buyerTypeMapping
     *          mapping for buyer type
     * @param buyerActivityTypeMapping
     *          mapping for buyer activity
     * @param countryMapping
     *            country mapping
     * @return cleaned body
     */
    public static CleanBody cleanBody(final ParsedBody parsedBody,
                                      final Map<Enum, List<String>> buyerTypeMapping,
                                      final Map<Enum, List<String>> buyerActivityTypeMapping,
                                      final Map<Enum, List<String>> countryMapping) {
        if (parsedBody == null) {
            return null;
        }

        return cleanBodyWithoutAddress(parsedBody, buyerTypeMapping, buyerActivityTypeMapping)
                .setAddress(AddressUtils.cleanAddress(parsedBody.getAddress(), countryMapping));
    }

    /**
     * Cleans all attributes except address in provided parsed body.
     *
     * @param parsedBody
     *          parsed body
     * @param buyerTypeMapping
     *          mapping for buyer type
     * @param buyerActivityTypeMapping
     *          mapping for buyer activity
     * @return cleaned body
     */
    private static CleanBody cleanBodyWithoutAddress(final ParsedBody parsedBody,
                                                     final Map<Enum, List<String>> buyerTypeMapping,
                                                     final Map<Enum, List<String>> buyerActivityTypeMapping) {
        if (parsedBody == null) {
            return null;
        }

        return new CleanBody()
                .setBodyIds(ArrayUtils.walk(parsedBody.getBodyIds(),
                        bodyId -> BodyIdentifierUtils.cleanBodyIdentifier(bodyId)))
                .setBuyerType(
                        (BuyerType) CodeTableUtils.mapValue(parsedBody.getBuyerType(), buyerTypeMapping,
                                BuyerType.OTHER))
                .setContactName(StringUtils.cleanShortString(parsedBody.getContactName()))
                .setContactPoint(StringUtils.cleanShortString(parsedBody.getContactPoint()))
                .setEmail(StringUtils.cleanShortString(parsedBody.getEmail()))
                .setIsLeader(StringUtils.cleanBoolean(parsedBody.getIsLeader()))
                .setIsPublic(StringUtils.cleanBoolean(parsedBody.getIsPublic()))
                .setIsSectoral(StringUtils.cleanBoolean(parsedBody.getIsSectoral()))
                .setIsSme(StringUtils.cleanBoolean(parsedBody.getIsSme()))
                .setIsSubsidized(StringUtils.cleanBoolean(parsedBody.getIsSubsidized()))
                .setMainActivities(ArrayUtils.walk(parsedBody.getMainActivities(),
                        (mainActivity) -> (BuyerActivityType) CodeTableUtils.mapValue(mainActivity,
                                buyerActivityTypeMapping, BuyerActivityType.OTHER)))
                .setName(StringUtils.cleanShortString(parsedBody.getName()))
                .setPhone(StringUtils.cleanShortString(parsedBody.getPhone()));
    }
}
