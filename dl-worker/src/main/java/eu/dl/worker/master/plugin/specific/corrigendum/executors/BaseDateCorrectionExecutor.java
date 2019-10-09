package eu.dl.worker.master.plugin.specific.corrigendum.executors;

import eu.dl.dataaccess.dto.generic.Corrigendum;

import java.time.LocalDateTime;

/**
 * Base correction executor which applies corrections of datetime fields.
 */
public abstract class BaseDateCorrectionExecutor extends BaseCorrectionExecutor<LocalDateTime> {
    @Override
    protected final LocalDateTime getCorrectionOrigin(final Corrigendum c) {
        return c.getOriginalDate();
    }

    @Override
    protected final LocalDateTime getCorrectionReplacement(final Corrigendum c) {
        return c.getReplacementDate();
    }

    /**
     * Checks if dates are equal or in case o2's localTime is 00:00 (when time was not set in the form) compares only localDates.
     *
     * @param o1 master tender
     * @param o2 master tender
     * @return true if objects are equal (or only their localDates) and false in other case.
     */
    @Override
    protected final boolean isEqual(final LocalDateTime o1, final LocalDateTime o2) {
        if (o1.isEqual(o2) || (o2.toLocalTime().toString().equals("00:00") && o2.toLocalDate().equals(o1.toLocalDate()))) {
            return true;
        }
        return false;
    }
}
