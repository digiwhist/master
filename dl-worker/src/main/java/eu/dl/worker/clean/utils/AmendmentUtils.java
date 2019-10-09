package eu.dl.worker.clean.utils;

import eu.dl.dataaccess.dto.generic.Amendment;
import eu.dl.dataaccess.dto.parsed.ParsedAmendment;

import java.text.NumberFormat;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.List;

/**
 * This class provide method for corrigendum cleaning.
 *
 * @author Tomas Mrazek
 */
public final class AmendmentUtils {

    /**
     * Utility classes should not have default constructor.
     */
    private AmendmentUtils() {

    }

    /**
     * Cleans the given corrigendum.
     *
     * @param amendment
     *         parsed amendment
     * @param numberFormat
     *         list of number formats
     * @param formatters
     *         datetime formatters
     * @param country
     *          country
     * @return cleaned amendment
     */
    public static Amendment cleanAmendment(final ParsedAmendment amendment,
            final List<NumberFormat> numberFormat, final List<DateTimeFormatter> formatters, final String country) {
        if (amendment == null) {
            return null;
        }

        return new Amendment()
                .setAddressOfImplementation(AddressUtils.cleanAddress(amendment.getAddressOfImplementation()))
                .setCpvs(CPVUtils.cleanCpvs(amendment.getCpvs()))
                .setDescription(StringUtils.cleanLongString(amendment.getDescription()))
                .setEstimatedCompletionDate(DateUtils.cleanDate(amendment.getEstimatedCompletionDate(), formatters))
                .setEstimatedDurationInDays(NumberUtils.cleanInteger(amendment.getEstimatedDurationInDays(), numberFormat))
                .setEstimatedDurationInMonths(NumberUtils.cleanInteger(amendment.getEstimatedDurationInMonths(), numberFormat))
                .setEstimatedDurationInYears(NumberUtils.cleanInteger(amendment.getEstimatedDurationInYears(), numberFormat))
                .setEstimatedStartDate(DateUtils.cleanDate(amendment.getEstimatedStartDate(), formatters))
                .setExcessiveFrameworkAgreementJustification(
                        StringUtils.cleanLongString(amendment.getExcessiveFrameworkAgreementJustification()))
                .setOriginalPrice(PriceUtils.cleanPrice(amendment.getOriginalPrice(), numberFormat, country))
                .setUpdatedPrice(PriceUtils.cleanPrice(amendment.getUpdatedPrice(), numberFormat, country))
                .setModificationReason(StringUtils.cleanLongString(amendment.getModificationReason()))
                .setModificationShortDescription(StringUtils.cleanLongString(amendment.getModificationShortDescription()))
                .setModificationReasonDescription(StringUtils.cleanLongString(amendment.getModificationReasonDescription()));
    }

    /**
     * Cleans the given corrigendum.
     *
     * @param amendment
     *         parsed amendment
     * @param numberFormat
     *         number format
     * @param formatter
     *         datetime formatter
     * @param country
     *          country
     * @return cleaned amendment
     */
    public static Amendment cleanAmendment(final ParsedAmendment amendment,
            final NumberFormat numberFormat, final List<DateTimeFormatter> formatter, final String country) {
        return cleanAmendment(amendment, Arrays.asList(numberFormat), formatter, country);
    }
}
