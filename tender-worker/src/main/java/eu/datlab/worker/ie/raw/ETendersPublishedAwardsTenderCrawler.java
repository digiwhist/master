package eu.datlab.worker.ie.raw;

import java.time.LocalDate;
import java.time.Month;

/**
 * Tender crawler for Ireland ETenders - Published awards.
 *
 * @author Marek Mikes
 */
public final class ETendersPublishedAwardsTenderCrawler extends BaseETendersTenderCrawler {
    private static final LocalDate OLDEST_NOTICE_DATE = LocalDate.of(2004, Month.FEBRUARY, 11);

    // Published awards
    private static final String FORM_TYPE_OPTION_TEXT = "2";

    @Override
    protected LocalDate getDefaultStartDate() {
        return OLDEST_NOTICE_DATE;
    }

    @Override
    protected String getFormTypeOptionText() {
        return FORM_TYPE_OPTION_TEXT;
    }
}
