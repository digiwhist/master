package eu.dl.worker.clean.utils;


import eu.dl.dataaccess.dto.generic.AwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;

import java.math.BigDecimal;
import java.text.NumberFormat;
import java.util.Arrays;
import java.util.List;


/**
 * This class provide method for address cleaning.
 *
 * @author Tomas Mrazek
 */
public final class AwardCriterionUtils {

    /**
     * Utility classes should not have default constructor.
     */
    private AwardCriterionUtils() {

    }

    /**
     * Cleans the given award criterion.
     *
     * @param parsedCriterion
     *            parsed award criterion
     * @param format
     *          list of number formats
     * @param weightMultiplier
     *          multiplier for weight
     * @return cleaned award criterion
     */
    public static AwardCriterion cleanAwardCriterion(final ParsedAwardCriterion parsedCriterion,
        final List<NumberFormat> format, final BigDecimal weightMultiplier) {
        if (parsedCriterion == null) {
            return null;
        }

        return new AwardCriterion()
            .setDescription(StringUtils.cleanLongString(parsedCriterion.getDescription()))
            .setIsPriceRelated(StringUtils.cleanBoolean(parsedCriterion.getIsPriceRelated()))
            .setName(StringUtils.cleanShortString(parsedCriterion.getName()))
            .setWeight(cleanWeight(parsedCriterion.getWeight(), format, weightMultiplier));
    }

    /**
     * Cleans the given award criterion.
     *
     * @param parsedCriterion
     *            parsed award criterion
     * @param format
     *          number format
     * @param weightMultiplier
     *          multiplier for weight
     * @return cleaned award criterion
     */
    public static AwardCriterion cleanAwardCriterion(final ParsedAwardCriterion parsedCriterion,
        final NumberFormat format, final BigDecimal weightMultiplier) {

        return cleanAwardCriterion(parsedCriterion, Arrays.asList(format), weightMultiplier);
    }

    /**
     * Award criteria weight is not always in percentage, make multiplier to get the sum closest possible to 100.
     *
     * @param awardCriteria award criteria
     * @param format number format
     * @return BigDecimal or null
     */
    public static BigDecimal countWeightMultiplier(final List<ParsedAwardCriterion> awardCriteria,
                                                   final List<NumberFormat> format) {
        if (awardCriteria == null) {
            return null;
        }

        Long weightSum = 0L;

        for (ParsedAwardCriterion criterion : awardCriteria) {
            final BigDecimal tempWeight = NumberUtils.cleanBigDecimal(criterion.getWeight(), format);
            if (tempWeight != null) {
                weightSum = weightSum + tempWeight.longValue();
            }
        }

        if (weightSum <= 1) {
            return BigDecimal.valueOf(100);
        } else if (weightSum <= 10) {
            return BigDecimal.valueOf(10);
        } else {
            return BigDecimal.valueOf(1);
        }
    }

    /**
     * Weight is not always in percentage, multiply number by predefined multiplier to fix that.
     *
     * @param weight criterion weight
     * @param format number format
     * @param weightMultiplier weight multiplier
     * @return Integer or null
     */
    private static Integer cleanWeight(final String weight, final List<NumberFormat> format, final BigDecimal
            weightMultiplier) {
        final BigDecimal rawWeight = NumberUtils.cleanBigDecimal(weight, format);

        return rawWeight == null ? null : rawWeight.multiply(weightMultiplier).intValue();
    }
}
