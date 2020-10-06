package eu.dl.worker.indicator.plugin;

import eu.dl.core.config.Config;
import eu.dl.core.config.MisconfigurationException;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.master.MasterTender;

/**
 * Utils class for indicator calculations.
 */
public final class IndicatorUtils {

    /**
     * Constructor.
     */
    private IndicatorUtils() {
        // utility classes do not allow instances
    }

    /**
     * Checks whether the periodLength is in period described by periodDescription
     * i.e. 12 is in 0-15;23-58.
     *
     * @param periodDescription periods described as 0-12;45-60 etc.
     * @param periodLength period length
     *
     * @return true if the value fits in at least one period
     */
    public static Boolean isValueInPeriod(final String periodDescription, final Long periodLength) {
        if (periodDescription != null && !periodDescription.isEmpty()) {
            String[] periods = periodDescription.split("\\;");
            for (String period : periods) {
                String[] interval = period.split("-");
                if (interval.length == 2) {
                    if (periodLength >= Long.valueOf(interval[0]) && periodLength <= Long.valueOf(interval[1])) {
                        return true;
                    }
                } else {
                    throw new MisconfigurationException("Unable to parse " + period + " into interval");
                }
            }
        }

        return false;
    }

    /**
     * Checks whether missing bidDeadline means red flag for a given country.
     *
     * @param country
     *      country code
     * @return true if missing bid deadline is considered red flag
     */
    public static boolean isMissingBidDeadlineRedFlag(final String country) {
        String value = Config.getInstance().getParam("indicator." + country + ".decisionPeriod.missing");
        return value != null && value.equals("1");
    }

    /**
     * Checks whether the decision period is of problematic length.
     *
     * @param countryCode
     *      country code
     * @param periodLength
     *      decision period length
     *
     * @return true if the decision period length is considered problematic
     */
    public static boolean checkDecisionPeriod(final String countryCode, final Long periodLength) {
        String value = Config.getInstance().getParam("indicator." + countryCode + ".decisionPeriod.length");
        return IndicatorUtils.isValueInPeriod(value, periodLength);
    }

    /**
     * @param tender
     *      master tender
     * @return true if the tender includes at least one included CONTRACT_AWARD publication, otherwise false
     */
    public static boolean hasAward(final MasterTender tender) {
        if (tender.getPublications() == null) {
            return false;
        }

        return tender.getPublications().stream()
            .filter(n -> Boolean.TRUE.equals(n.getIsIncluded()))
            .anyMatch(n -> n.getFormType() == PublicationFormType.CONTRACT_AWARD);
    }
}
