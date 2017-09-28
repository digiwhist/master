package eu.dl.worker.indicator.plugin;

import eu.dl.core.config.MisconfigurationException;

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
}
