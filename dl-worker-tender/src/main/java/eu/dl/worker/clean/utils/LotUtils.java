package eu.dl.worker.clean.utils;

import eu.dl.worker.utils.ArrayUtils;
import eu.dl.dataaccess.dto.clean.CleanTenderLot;
import eu.dl.dataaccess.dto.codetables.TenderLotStatus;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

/**
 * This class provide method for CPV cleaning.
 *
 * @author Tomas Mrazek
 */
public final class LotUtils {
    private static final Integer BIDS_COUNT_MIN = 0;
    private static final Integer BIDS_COUNT_MAX = 999;

    /**
     * Utility classes should not have default constructor.
     */
    private LotUtils() {

    }

    /**
     * Cleans the given lot.
     *
     * @param parsedLot
     *         parsed lot
     * @param numberFormat
     *         list of number formats
     * @param formatter
     *         datetime formatter
     * @param lotMappings
     *         lot mappings
     * @param country
     *          country
     * @return cleaned lot
     */
    public static CleanTenderLot cleanLot(final ParsedTenderLot parsedLot, final List<NumberFormat> numberFormat,
            final List<DateTimeFormatter> formatter, final Map<String, Map<Enum, List<String>>> lotMappings,
            final String country) {
        if (parsedLot == null) {
            return null;
        }

        Map<Enum, List<String>> documentTypeMapping = lotMappings != null ? lotMappings.get("documentTypeMapping") : null;
        Map<Enum, List<String>> unitPriceMapping = lotMappings != null ? lotMappings.get("unitPriceMapping") : null;
        Map<Enum, List<String>> countryMapping = lotMappings != null ? lotMappings.get("countryMapping") : null;
        Map<Enum, List<String>> selectionMethodMapping = lotMappings != null ? lotMappings.get("selectionMethodMapping") : null;
        Map<Enum, List<String>> statusMapping = lotMappings != null ? lotMappings.get("statusMapping") : null;

        return new CleanTenderLot()
            //cleaning of the lotId isn't needed but is necessary to set it
            .setLotId(parsedLot.getLotId())
            .setAddressOfImplementation(AddressUtils.cleanAddress(parsedLot.getAddressOfImplementation()))
            .setAwardCriteria(ArrayUtils.walk(parsedLot.getAwardCriteria(),
                (parsedCriterion) -> AwardCriterionUtils.cleanAwardCriterion(parsedCriterion, numberFormat,
                    AwardCriterionUtils.countWeightMultiplier(parsedLot.getAwardCriteria(), numberFormat))))
            .setAwardDecisionDate(DateUtils.cleanDate(parsedLot.getAwardDecisionDate(), formatter))
            .setBids(ArrayUtils.walk(parsedLot.getBids(),
                (parsedBid) -> BidUtils.cleanBid(parsedBid, numberFormat, formatter, documentTypeMapping, unitPriceMapping, countryMapping,
                    country)))
            .setBidsCount(LotUtils.removeNonsensicalBidsCount(parsedLot.getBidsCount(), numberFormat))
            .setCancellationDate(DateUtils.cleanDate(parsedLot.getCancellationDate(), formatter))
            .setCancellationReason(StringUtils.cleanLongString(parsedLot.getCancellationReason()))
            .setCompletionDate(DateUtils.cleanDate(parsedLot.getCompletionDate(), formatter))
            .setContractNumber(StringUtils.cleanShortString(parsedLot.getContractNumber()))
            .setContractSignatureDate(DateUtils.cleanDate(parsedLot.getContractSignatureDate(), formatter))
            .setCpvs(CPVUtils.cleanCpvs(parsedLot.getCpvs()))
            .setDescription(StringUtils.cleanLongString(parsedLot.getDescription()))
            .setDescriptionEnglish(StringUtils.cleanLongString(parsedLot.getDescriptionEnglish()))
            .setElectronicBidsCount(NumberUtils.cleanInteger(parsedLot.getElectronicBidsCount(), numberFormat))
            .setEligibilityCriteria(StringUtils.cleanLongString(parsedLot.getEligibilityCriteria()))
            .setEstimatedCompletionDate(DateUtils.cleanDate(parsedLot.getEstimatedCompletionDate(), formatter))
            .setEstimatedDurationInDays(NumberUtils.cleanInteger(parsedLot.getEstimatedDurationInDays(), numberFormat))
            .setEstimatedDurationInMonths(NumberUtils.cleanInteger(parsedLot.getEstimatedDurationInMonths(), numberFormat))
            .setEstimatedPrice(PriceUtils.cleanPrice(parsedLot.getEstimatedPrice(), numberFormat, country))
            .setEstimatedStartDate(DateUtils.cleanDate(parsedLot.getEstimatedStartDate(), formatter))
            .setForeignCompaniesBidsCount(NumberUtils.cleanInteger(parsedLot.getForeignCompaniesBidsCount(), numberFormat))
            .setFundings(ArrayUtils.walk(parsedLot.getFundings(),
                (parsedFunding) -> FundingUtils.cleanFunding(parsedFunding, numberFormat, country)))
            .setIsAwarded(StringUtils.cleanBoolean(parsedLot.getIsAwarded()))
            .setIsCoveredByGpa(StringUtils.cleanBoolean(parsedLot.getIsCoveredByGpa()))
            .setIsDps(StringUtils.cleanBoolean(parsedLot.getIsDps()))
            .setIsElectronicAuction(StringUtils.cleanBoolean(parsedLot.getIsElectronicAuction()))
            .setIsFrameworkAgreement(StringUtils.cleanBoolean(parsedLot.getIsFrameworkAgreement()))
            .setLotNumber(NumberUtils.cleanInteger(parsedLot.getLotNumber(), numberFormat))
            .setMaxFrameworkAgreementParticipants(NumberUtils.cleanInteger(parsedLot.getMaxFrameworkAgreementParticipants(), numberFormat))
            .setNonEuMemberStatesCompaniesBidsCount(
                NumberUtils.cleanInteger(parsedLot.getNonEuMemberStatesCompaniesBidsCount(), numberFormat))
            .setOtherEuMemberStatesCompaniesBidsCount(
                NumberUtils.cleanInteger(parsedLot.getOtherEuMemberStatesCompaniesBidsCount(), numberFormat))
            .setPositionOnPage(NumberUtils.cleanInteger(parsedLot.getPositionOnPage(), numberFormat))
            .setSelectionMethod(SelectionMethodUtils.cleanSelectionMethod(parsedLot.getSelectionMethod(), selectionMethodMapping))
            .setSmeBidsCount(NumberUtils.cleanInteger(parsedLot.getSmeBidsCount(), numberFormat))
            .setStatus((TenderLotStatus) CodeTableUtils.mapValue(parsedLot.getStatus(), statusMapping))
            .setTitle(StringUtils.cleanShortString(parsedLot.getTitle()))
            .setTitleEnglish(StringUtils.cleanShortString(parsedLot.getTitleEnglish()))
            .setValidBidsCount(NumberUtils.cleanInteger(parsedLot.getValidBidsCount(), numberFormat));
    }

    /**
     * @param bidsCount
     *         parsed bids count
     * @param numberFormat
     *         list of number formats
     * @param min
     *          minimal accepted value
     * @param max
     *          maximal accepted value
     * @return bids count if the value is in range given by min and max, otherwise null
     */
    public static Integer removeNonsensicalBidsCount(final String bidsCount, final List<NumberFormat> numberFormat,
        final int min, final int max) {
        Integer count = NumberUtils.cleanInteger(bidsCount, numberFormat);
        if (count != null && (count < min || count > max)) {
            return null;
        }

        return count;
    }

    /**
     * @param bidsCount
     *         parsed bids count
     * @param numberFormat
     *         list of number formats
     * @return bids count if the value is in range given by BIDS_COUNT_MIN and BIDS_COUNT_MAX, otherwise null
     */
    public static Integer removeNonsensicalBidsCount(final String bidsCount, final List<NumberFormat> numberFormat) {
        return removeNonsensicalBidsCount(bidsCount, numberFormat, BIDS_COUNT_MIN, BIDS_COUNT_MAX);
    }

    /**
     * Cleans the given lot.
     *
     * @param parsedLot
     *         parsed lot
     * @param numberFormat
     *         number format
     * @param formatter
     *         datetime formatter
     * @param lotMappings
     *         lot mappings
     * @param country
     *          country
     * @return cleaned lot
     */
    public static CleanTenderLot cleanLot(final ParsedTenderLot parsedLot, final NumberFormat numberFormat,
            final List<DateTimeFormatter> formatter, final Map<String, Map<Enum, List<String>>> lotMappings,
            final String country) {
        return cleanLot(parsedLot, Arrays.asList(numberFormat), formatter, lotMappings, country);
    }
}
