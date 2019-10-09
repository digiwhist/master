package eu.dl.worker.master.utils;

import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;

import java.time.temporal.ChronoUnit;

/**
 * Utils for Mastering.
 */
public final class MasterUtils {
    /**
     * amount of days in month (average).
     */
    private static final int DAYS_IN_MONTH = 30;
    /**
     * amount of days in year (average).
     */
    private static final int DAYS_IN_YEAR = 365;

    /**
     * Private constructor.
     */
    private MasterUtils() {
    }

    /**
     * This method calculates EstimatedDurationInDays by
     * . case 1: EstimatedCompletionDate and EstimatedStartDate
     * . case 2: EstimatedDurationInMonths
     * . case 3: EstimatedDurationInYears
     * . case 4: do nothing (masterTender hasn`t any information mentioned above)
     *
     * @param masterTender actual tender
     */
    public static void calculateEstimatedDurationInDays(final MasterTender masterTender) {

        // in case masterTender already has estimatedDurationInDays
        if (masterTender.getEstimatedDurationInDays() != null) {
            return;
        }
        if (masterTender.getLots() != null) {
            for (MasterTenderLot lot : masterTender.getLots()) {
                if (lot.getEstimatedDurationInDays() != null) {
                    return;
                }
            }
        }

        // case 1
        if (masterTender.getEstimatedCompletionDate() != null
                && masterTender.getEstimatedStartDate() != null) {
            int period = (int) ChronoUnit.DAYS.between(masterTender.getEstimatedStartDate(),
                    masterTender.getEstimatedCompletionDate());
            if (period >= 0) {
                masterTender.setEstimatedDurationInDays(period);
            }
            return;
        }
        if (masterTender.getLots() != null) {
            for (MasterTenderLot lot : masterTender.getLots()) {
                if (lot.getEstimatedCompletionDate() != null
                        && lot.getEstimatedStartDate() != null) {
                    int period = (int) ChronoUnit.DAYS.between(lot.getEstimatedStartDate(),
                            lot.getEstimatedCompletionDate());
                    if (period >= 0) {
                        lot.setEstimatedDurationInDays(period);
                    }
                    return;
                }
            }
        }

        // case 2
        if (masterTender.getEstimatedDurationInMonths() != null) {
            masterTender.setEstimatedDurationInDays(
                    masterTender.getEstimatedDurationInMonths() * DAYS_IN_MONTH);
            return;
        }
        if (masterTender.getLots() != null) {
            for (MasterTenderLot lot : masterTender.getLots()) {
                if (lot.getEstimatedDurationInMonths() != null) {
                    lot.setEstimatedDurationInDays(
                            lot.getEstimatedDurationInMonths() * DAYS_IN_MONTH);
                    return;
                }
            }
        }

        // case 3
        if (masterTender.getEstimatedDurationInYears() != null) {
            masterTender.setEstimatedDurationInDays(
                    masterTender.getEstimatedDurationInYears() * DAYS_IN_YEAR);
        }
        if (masterTender.getLots() != null) {
            for (MasterTenderLot lot : masterTender.getLots()) {
                if (lot.getEstimatedDurationInYears() != null) {
                    lot.setEstimatedDurationInDays(
                            lot.getEstimatedDurationInYears() * DAYS_IN_YEAR);
                }
            }
        }
        // case 4 (just nothing)
    }
}
