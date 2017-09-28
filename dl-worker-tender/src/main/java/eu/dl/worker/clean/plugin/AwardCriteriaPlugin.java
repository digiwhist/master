package eu.dl.worker.clean.plugin;

import java.text.NumberFormat;
import java.util.Arrays;
import java.util.List;

import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.utils.AwardCriterionUtils;
import eu.dl.worker.utils.ArrayUtils;

/**
 * Plugin used to clean tender award criteria.
 *
 * @author Tomas Mrazek
 */
public class AwardCriteriaPlugin extends BaseCleaningPlugin<ParsedTender, CleanTender> {

    private final List<NumberFormat> numberFormat;

    /**
     * AwardCriteriaPlugin should be initialised with the number format.
     *
     * @param format
     *      number format
     */
    public AwardCriteriaPlugin(final NumberFormat format) {
        this.numberFormat = Arrays.asList(format);
    }

    /**
     * AwardCriteriaPlugin should be initialised with the number format.
     *
     * @param format
     *      list of number formats
     */
    public AwardCriteriaPlugin(final List<NumberFormat> format) {
        this.numberFormat = format;
    }

    /**
     * Cleans award criteria.
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
        if (parsedTender.getAwardCriteria() != null) {
            logger.debug("Cleaning award criteria for parsed tender {} starts", parsedTender.getId());
            cleanTender.setAwardCriteria(ArrayUtils.walk(parsedTender.getAwardCriteria(),
                (parsedCriterion) -> AwardCriterionUtils.cleanAwardCriterion(parsedCriterion, numberFormat,
                        AwardCriterionUtils.countWeightMultiplier(parsedTender.getAwardCriteria(), numberFormat))));
            logger.debug("Cleaning award criteria for parsed tender {} finished", parsedTender.getId());
        }

        return cleanTender;
    }
}
