package eu.dl.worker.master.utils;


import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import org.junit.Test;

import java.time.LocalDate;

/**
 * Tests for MasterUtils.
 */
public final class MasterUtilsTest {
    /**
     * Some date for tests (actual date of testing) : 2019.7.23.
     */
    private final LocalDate someDate = LocalDate.of(2019, 7, 23);

    /**
     * Some another date : 2019.8.1 .
     */
    private final LocalDate someAnotherDate = LocalDate.of(2019, 8, 1);


    private final MasterTender attrTender = new MasterTender();


    /**
     * Cases when EstimatedDurationInDays already exist and MasterTender hasn`t enough information.
     */
    @Test
    public void doNothingTest() {
        MasterUtils.calculateEstimatedDurationInDays(attrTender);
        assert (attrTender.getEstimatedStartDate() == null
                && attrTender.getEstimatedDurationInYears() == null
                && attrTender.getEstimatedDurationInMonths() == null
                && attrTender.getEstimatedDurationInDays() == null
                && attrTender.getEstimatedCompletionDate() == null)
                : printTenderToString(attrTender);

        attrTender.setEstimatedDurationInDays(10);
        MasterUtils.calculateEstimatedDurationInDays(attrTender);
        assert (attrTender.getEstimatedStartDate() == null
                && attrTender.getEstimatedDurationInYears() == null
                && attrTender.getEstimatedDurationInMonths() == null
                && attrTender.getEstimatedDurationInDays() == 10
                && attrTender.getEstimatedCompletionDate() == null)
                : printTenderToString(attrTender);
        resetTender();
    }

    /**
     * Case when MasterTender has date of start and date of completion.
     */
    @Test
    public void startAndComletionDatesTest() {
        attrTender.setEstimatedStartDate(someDate);
        attrTender.setEstimatedCompletionDate(someDate);
        MasterUtils.calculateEstimatedDurationInDays(attrTender);
        assert (attrTender.getEstimatedStartDate() == someDate
                && attrTender.getEstimatedDurationInYears() == null
                && attrTender.getEstimatedDurationInMonths() == null
                && attrTender.getEstimatedDurationInDays() != null
                && attrTender.getEstimatedDurationInDays() == 0
                && attrTender.getEstimatedCompletionDate() == someDate)
                : printTenderToString(attrTender);
        resetTender();
        attrTender.setEstimatedStartDate(someDate);
        attrTender.setEstimatedCompletionDate(someAnotherDate);
        MasterUtils.calculateEstimatedDurationInDays(attrTender);
        assert (attrTender.getEstimatedStartDate() == someDate
                && attrTender.getEstimatedDurationInYears() == null
                && attrTender.getEstimatedDurationInMonths() == null
                && attrTender.getEstimatedDurationInDays() != null
                && attrTender.getEstimatedDurationInDays() == 9
                && attrTender.getEstimatedCompletionDate() == someAnotherDate)
                : printTenderToString(attrTender);
        resetTender();
        attrTender.setEstimatedStartDate(someAnotherDate);
        attrTender.setEstimatedCompletionDate(someDate);
        MasterUtils.calculateEstimatedDurationInDays(attrTender);
        assert (attrTender.getEstimatedStartDate() == someAnotherDate
                && attrTender.getEstimatedDurationInYears() == null
                && attrTender.getEstimatedDurationInMonths() == null
                && attrTender.getEstimatedDurationInDays() == null
                && attrTender.getEstimatedCompletionDate() == someDate)
                : printTenderToString(attrTender);
        resetTender();
    }

    /**
     * Case when MasterTender has durationInMonth.
     */
    @Test
    public void durationInMonthTest() {
        attrTender.setEstimatedDurationInMonths(2);
        MasterUtils.calculateEstimatedDurationInDays(attrTender);
        assert (attrTender.getEstimatedStartDate() == null
                && attrTender.getEstimatedDurationInYears() == null
                && attrTender.getEstimatedDurationInMonths() == 2
                && attrTender.getEstimatedDurationInDays() != null
                && attrTender.getEstimatedDurationInDays() == 60
                && attrTender.getEstimatedCompletionDate() == null)
                : printTenderToString(attrTender);
        resetTender();
        attrTender.setEstimatedDurationInMonths(0);
        MasterUtils.calculateEstimatedDurationInDays(attrTender);
        assert (attrTender.getEstimatedStartDate() == null
                && attrTender.getEstimatedDurationInYears() == null
                && attrTender.getEstimatedDurationInMonths() == 0
                && attrTender.getEstimatedDurationInDays() != null
                && attrTender.getEstimatedDurationInDays() == 0
                && attrTender.getEstimatedCompletionDate() == null)
                : printTenderToString(attrTender);
        resetTender();
    }

    /**
     * Case when MasterTender has durationInYear.
     */
    @Test
    public void durationInYearTest() {
        attrTender.setEstimatedDurationInYears(1);
        MasterUtils.calculateEstimatedDurationInDays(attrTender);
        assert (attrTender.getEstimatedStartDate() == null
                && attrTender.getEstimatedDurationInYears() == 1
                && attrTender.getEstimatedDurationInMonths() == null
                && attrTender.getEstimatedDurationInDays() != null
                && attrTender.getEstimatedDurationInDays() == 365
                && attrTender.getEstimatedCompletionDate() == null)
                : printTenderToString(attrTender);
        resetTender();
        attrTender.setEstimatedDurationInYears(0);
        MasterUtils.calculateEstimatedDurationInDays(attrTender);
        assert (attrTender.getEstimatedStartDate() == null
                && attrTender.getEstimatedDurationInYears() == 0
                && attrTender.getEstimatedDurationInMonths() == null
                && attrTender.getEstimatedDurationInDays() != null
                && attrTender.getEstimatedDurationInDays() == 0
                && attrTender.getEstimatedCompletionDate() == null)
                : printTenderToString(attrTender);
        resetTender();
    }

    /**
     * Test on priority of calculations.
     */
    @Test
    public void priorityTest() {
        attrTender.setEstimatedDurationInMonths(15);
        attrTender.setEstimatedDurationInYears(1);
        MasterUtils.calculateEstimatedDurationInDays(attrTender);
        assert (attrTender.getEstimatedStartDate() == null
                && attrTender.getEstimatedDurationInYears() == 1
                && attrTender.getEstimatedDurationInMonths() == 15
                && attrTender.getEstimatedDurationInDays() != null
                && attrTender.getEstimatedDurationInDays() == 450
                && attrTender.getEstimatedCompletionDate() == null)
                : printTenderToString(attrTender);
        resetTender();
        attrTender.setEstimatedDurationInMonths(0);
        attrTender.setEstimatedDurationInYears(0);
        attrTender.setEstimatedStartDate(someDate);
        attrTender.setEstimatedCompletionDate(someAnotherDate);
        MasterUtils.calculateEstimatedDurationInDays(attrTender);
        assert (attrTender.getEstimatedStartDate() == someDate
                && attrTender.getEstimatedDurationInYears() == 0
                && attrTender.getEstimatedDurationInMonths() == 0
                && attrTender.getEstimatedDurationInDays() != null
                && attrTender.getEstimatedDurationInDays() == 9
                && attrTender.getEstimatedCompletionDate() == someAnotherDate)
                : printTenderToString(attrTender);
        resetTender();
    }
    //----------------------------------------------------------------------------------

    /**
     * Cases when EstimatedDurationInDays in lot already exist and MasterTender hasn`t enough information.
     */
    @Test
    public void lotsDoNothingTest() {
        attrTender.addLot(new MasterTenderLot());
        MasterUtils.calculateEstimatedDurationInDays(attrTender);
        assert (attrTender.getLots().get(0).getEstimatedStartDate() == null
                && attrTender.getLots().get(0).getEstimatedDurationInYears() == null
                && attrTender.getLots().get(0).getEstimatedDurationInMonths() == null
                && attrTender.getLots().get(0).getEstimatedDurationInDays() == null
                && attrTender.getLots().get(0).getEstimatedCompletionDate() == null)
                : printTendersLotsToString(attrTender);

        attrTender.getLots().get(0).setEstimatedDurationInDays(10);
        MasterUtils.calculateEstimatedDurationInDays(attrTender);
        assert (attrTender.getLots().get(0).getEstimatedStartDate() == null
                && attrTender.getLots().get(0).getEstimatedDurationInYears() == null
                && attrTender.getLots().get(0).getEstimatedDurationInMonths() == null
                && attrTender.getLots().get(0).getEstimatedDurationInDays() == 10
                && attrTender.getLots().get(0).getEstimatedCompletionDate() == null)
                : printTendersLotsToString(attrTender);
        attrTender.setLots(null);
        attrTender.addLot(new MasterTenderLot());
        attrTender.addLot(new MasterTenderLot());
        attrTender.getLots().get(0).setEstimatedStartDate(someDate);
        attrTender.getLots().get(1).setEstimatedCompletionDate(someAnotherDate);
        MasterUtils.calculateEstimatedDurationInDays(attrTender);
        assert (attrTender.getLots().get(0).getEstimatedStartDate() == someDate
                && attrTender.getLots().get(0).getEstimatedDurationInYears() == null
                && attrTender.getLots().get(0).getEstimatedDurationInMonths() == null
                && attrTender.getLots().get(0).getEstimatedDurationInDays() == null
                && attrTender.getLots().get(0).getEstimatedCompletionDate() == null)
                : printTendersLotsToString(attrTender);
        assert (attrTender.getLots().get(1).getEstimatedStartDate() == null
                && attrTender.getLots().get(1).getEstimatedDurationInYears() == null
                && attrTender.getLots().get(1).getEstimatedDurationInMonths() == null
                && attrTender.getLots().get(1).getEstimatedDurationInDays() == null
                && attrTender.getLots().get(1).getEstimatedCompletionDate() == someAnotherDate)
                : printTendersLotsToString(attrTender);
        resetTender();
    }

    /**
     * Case when MasterTender`s lot has date of start and date of completion.
     */
    @Test
    public void lotsStartAndComletionDatesTest() {
        attrTender.addLot(new MasterTenderLot());
        attrTender.addLot(new MasterTenderLot());
        attrTender.getLots().get(1).setEstimatedStartDate(someDate);
        attrTender.getLots().get(1).setEstimatedCompletionDate(someAnotherDate);
        MasterUtils.calculateEstimatedDurationInDays(attrTender);
        assert (attrTender.getLots().get(0).getEstimatedStartDate() == null
                && attrTender.getLots().get(0).getEstimatedDurationInYears() == null
                && attrTender.getLots().get(0).getEstimatedDurationInMonths() == null
                && attrTender.getLots().get(0).getEstimatedDurationInDays() == null
                && attrTender.getLots().get(0).getEstimatedCompletionDate() == null)
                : printTendersLotsToString(attrTender);
        assert (attrTender.getLots().get(1).getEstimatedStartDate() == someDate
                && attrTender.getLots().get(1).getEstimatedDurationInYears() == null
                && attrTender.getLots().get(1).getEstimatedDurationInMonths() == null
                && attrTender.getLots().get(1).getEstimatedDurationInDays() != null
                && attrTender.getLots().get(1).getEstimatedDurationInDays() == 9
                && attrTender.getLots().get(1).getEstimatedCompletionDate() == someAnotherDate)
                : printTendersLotsToString(attrTender);
        resetTender();
        attrTender.setLots(null);

    }

    /**
     * Case when MasterTender`s lot has durationInMonth.
     */
    @Test
    public void lotsDurationInMonthTest() {
        attrTender.addLot(new MasterTenderLot());
        attrTender.getLots().get(0).setEstimatedDurationInMonths(2);
        MasterUtils.calculateEstimatedDurationInDays(attrTender);
        assert (attrTender.getLots().get(0).getEstimatedStartDate() == null
                && attrTender.getLots().get(0).getEstimatedDurationInYears() == null
                && attrTender.getLots().get(0).getEstimatedDurationInMonths() == 2
                && attrTender.getLots().get(0).getEstimatedDurationInDays() != null
                && attrTender.getLots().get(0).getEstimatedDurationInDays() == 60
                && attrTender.getLots().get(0).getEstimatedCompletionDate() == null)
                : printTendersLotsToString(attrTender);
        attrTender.setLots(null);
    }

    /**
     * Case when MasterTender has durationInYear.
     */
    @Test
    public void lotsDurationInYearTest() {
        attrTender.addLot(new MasterTenderLot());
        attrTender.getLots().get(0).setEstimatedDurationInYears(1);
        MasterUtils.calculateEstimatedDurationInDays(attrTender);
        assert (attrTender.getLots().get(0).getEstimatedStartDate() == null
                && attrTender.getLots().get(0).getEstimatedDurationInYears() == 1
                && attrTender.getLots().get(0).getEstimatedDurationInMonths() == null
                && attrTender.getLots().get(0).getEstimatedDurationInDays() != null
                && attrTender.getLots().get(0).getEstimatedDurationInDays() == 365
                && attrTender.getLots().get(0).getEstimatedCompletionDate() == null)
                : printTendersLotsToString(attrTender);
        attrTender.setLots(null);
    }

    /**
     * Test on priority of calculations.
     */
    @Test
    public void lotsPriorityTest() {
        attrTender.addLot(new MasterTenderLot());
        attrTender.getLots().get(0).setEstimatedDurationInMonths(15);
        attrTender.getLots().get(0).setEstimatedDurationInYears(1);
        MasterUtils.calculateEstimatedDurationInDays(attrTender);
        assert (attrTender.getLots().get(0).getEstimatedStartDate() == null
                && attrTender.getLots().get(0).getEstimatedDurationInYears() == 1
                && attrTender.getLots().get(0).getEstimatedDurationInMonths() == 15
                && attrTender.getLots().get(0).getEstimatedDurationInDays() != null
                && attrTender.getLots().get(0).getEstimatedDurationInDays() == 450
                && attrTender.getLots().get(0).getEstimatedCompletionDate() == null)
                : printTendersLotsToString(attrTender);
        attrTender.setLots(null);
    }


    /**
     * Method writes parametres into one String for printing on screen in case assert is true.
     *
     * @param tender MasterTender
     * @return string with actual parametres
     */
    private String printTenderToString(final MasterTender tender) {
        StringBuilder str = new StringBuilder();
        str.append("\nActual situation:\n");
        str.append("estimatedStartDate: ");
        if (tender.getEstimatedStartDate() != null) {
            str.append(tender.getEstimatedStartDate() + "\n");
        } else {
            str.append("null\n");
        }
        str.append("estimatedCompletionDate: ");
        if (tender.getEstimatedCompletionDate() != null) {
            str.append(tender.getEstimatedCompletionDate() + "\n");
        } else {
            str.append("null\n");
        }
        str.append("estimatedDurationInYears: ");
        if (tender.getEstimatedDurationInYears() != null) {
            str.append(tender.getEstimatedDurationInYears() + "\n");
        } else {
            str.append("null\n");
        }
        str.append("estimatedDurationInMonths: ");
        if (tender.getEstimatedDurationInMonths() != null) {
            str.append(tender.getEstimatedDurationInMonths() + "\n");
        } else {
            str.append("null\n");
        }
        str.append("estimatedDurationInDays: ");
        if (tender.getEstimatedDurationInDays() != null) {
            str.append(tender.getEstimatedDurationInDays() + "\n");
        } else {
            str.append("null\n");
        }
        return str.toString();
    }

    /**
     * Method writes parametres of each lot into one String for printing on screen in case assert is true.
     *
     * @param tender MasterTender
     * @return string with actual parametres of lots
     */
    private String printTendersLotsToString(final MasterTender tender) {
        StringBuilder str = new StringBuilder();
        if (tender.getLots() == null) {
            str.append("Theres is no lots");
            return str.toString();
        }
        for (MasterTenderLot lot : tender.getLots()) {
            str.append("\nActual situation:\n");
            str.append("estimatedStartDate: ");
            if (lot.getEstimatedStartDate() != null) {
                str.append(lot.getEstimatedStartDate() + "\n");
            } else {
                str.append("null\n");
            }
            str.append("estimatedCompletionDate: ");
            if (lot.getEstimatedCompletionDate() != null) {
                str.append(lot.getEstimatedCompletionDate() + "\n");
            } else {
                str.append("null\n");
            }
            str.append("estimatedDurationInYears: ");
            if (lot.getEstimatedDurationInYears() != null) {
                str.append(lot.getEstimatedDurationInYears() + "\n");
            } else {
                str.append("null\n");
            }
            str.append("estimatedDurationInMonths: ");
            if (lot.getEstimatedDurationInMonths() != null) {
                str.append(lot.getEstimatedDurationInMonths() + "\n");
            } else {
                str.append("null\n");
            }
            str.append("estimatedDurationInDays: ");
            if (lot.getEstimatedDurationInDays() != null) {
                str.append(lot.getEstimatedDurationInDays() + "\n");
            } else {
                str.append("null\n");
            }
        }
        return str.toString();
    }

    /**
     * resets all needed paramtetres in "attrTender".
     */
    private void resetTender() {
        attrTender.setEstimatedStartDate(null);
        attrTender.setEstimatedDurationInYears(null);
        attrTender.setEstimatedDurationInMonths(null);
        attrTender.setEstimatedDurationInDays(null);
        attrTender.setEstimatedCompletionDate(null);
    }


}
