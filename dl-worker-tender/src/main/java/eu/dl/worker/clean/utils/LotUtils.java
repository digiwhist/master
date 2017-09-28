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
     *
     * @return cleaned lot
     */
    public static CleanTenderLot cleanLot(final ParsedTenderLot parsedLot, final List<NumberFormat> numberFormat,
            final List<DateTimeFormatter> formatter, final Map<String, Map<Enum, List<String>>> lotMappings) {
        if (parsedLot == null) {
            return null;
        }

        return new CleanTenderLot()
                //cleaning of the lotId isn't needed but is necessary to set it
                .setLotId(parsedLot.getLotId())
                .setAddressOfImplementation(AddressUtils.cleanAddress(parsedLot.getAddressOfImplementation()))
                .setAwardCriteria(ArrayUtils.walk(parsedLot.getAwardCriteria(),
                        (parsedCriterion) -> AwardCriterionUtils.cleanAwardCriterion(parsedCriterion, numberFormat,
                                AwardCriterionUtils.countWeightMultiplier(parsedLot.getAwardCriteria(), numberFormat))))
                .setAwardDecisionDate(DateUtils.cleanDate(parsedLot.getAwardDecisionDate(), formatter))
                .setBids(ArrayUtils.walk(parsedLot.getBids(),
                        (parsedBid) -> BidUtils.cleanBid(parsedBid, numberFormat, formatter,
                                lotMappings.get("documentTypeMapping"), lotMappings.get("unitPriceMapping"),
                                lotMappings.get("countryMapping"))))
                .setBidsCount(NumberUtils.cleanInteger(parsedLot.getBidsCount(), numberFormat))
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
                .setEstimatedDurationInDays(
                        NumberUtils.cleanInteger(parsedLot.getEstimatedDurationInDays(), numberFormat))
                .setEstimatedDurationInMonths(
                        NumberUtils.cleanInteger(parsedLot.getEstimatedDurationInMonths(), numberFormat))
                .setEstimatedPrice(PriceUtils.cleanPrice(parsedLot.getEstimatedPrice(), numberFormat))
                .setEstimatedStartDate(DateUtils.cleanDate(parsedLot.getEstimatedStartDate(), formatter))
                .setForeignCompaniesBidsCount(
                        NumberUtils.cleanInteger(parsedLot.getForeignCompaniesBidsCount(), numberFormat))
                .setFundings(ArrayUtils.walk(parsedLot.getFundings(),
                        (parsedFunding) -> FundingUtils.cleanFunding(parsedFunding, numberFormat)))
                .setIsAwarded(StringUtils.cleanBoolean(parsedLot.getIsAwarded()))
                .setIsCoveredByGpa(StringUtils.cleanBoolean(parsedLot.getIsCoveredByGpa()))
                .setIsDps(StringUtils.cleanBoolean(parsedLot.getIsDps()))
                .setIsElectronicAuction(StringUtils.cleanBoolean(parsedLot.getIsElectronicAuction()))
                .setIsFrameworkAgreement(StringUtils.cleanBoolean(parsedLot.getIsFrameworkAgreement()))
                .setLotNumber(NumberUtils.cleanInteger(parsedLot.getLotNumber(), numberFormat))
                .setMaxFrameworkAgreementParticipants(
                        NumberUtils.cleanInteger(parsedLot.getMaxFrameworkAgreementParticipants(), numberFormat))
                .setNonEuMemberStatesCompaniesBidsCount(
                        NumberUtils.cleanInteger(parsedLot.getNonEuMemberStatesCompaniesBidsCount(), numberFormat))
                .setOtherEuMemberStatesCompaniesBidsCount(
                        NumberUtils.cleanInteger(parsedLot.getOtherEuMemberStatesCompaniesBidsCount(), numberFormat))
                .setPositionOnPage(NumberUtils.cleanInteger(parsedLot.getPositionOnPage(), numberFormat))
                .setSelectionMethod(SelectionMethodUtils.cleanSelectionMethod(parsedLot.getSelectionMethod(),
                        lotMappings.get("selectionMethodMapping")))
                .setSmeBidsCount(NumberUtils.cleanInteger(parsedLot.getSmeBidsCount(), numberFormat))
                .setStatus((TenderLotStatus) CodeTableUtils.mapValue(parsedLot.getStatus(),
                        lotMappings.get("statusMapping")))
                .setTitle(StringUtils.cleanShortString(parsedLot.getTitle()))
                .setTitleEnglish(StringUtils.cleanShortString(parsedLot.getTitleEnglish()))
                .setValidBidsCount(NumberUtils.cleanInteger(parsedLot.getValidBidsCount(), numberFormat));
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
     *
     * @return cleaned lot
     */
    public static CleanTenderLot cleanLot(final ParsedTenderLot parsedLot, final NumberFormat numberFormat,
            final List<DateTimeFormatter> formatter, final Map<String, Map<Enum, List<String>>> lotMappings) {
        return cleanLot(parsedLot, Arrays.asList(numberFormat), formatter, lotMappings);
    }
}
