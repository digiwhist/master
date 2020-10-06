package eu.datlab.worker.ie.raw;

import java.time.LocalDate;
import java.time.Month;

/**
 * Tender crawler for Ireland ETenders - Published tenders.
 *
 * @author Marek Mikes
 */
public final class ETendersPublishedTendersTenderCrawler extends BaseETendersTenderCrawler {
    private static final LocalDate OLDEST_NOTICE_DATE = LocalDate.of(2004, Month.JANUARY, 20);

    private static final String FORM_TYPE_OPTION_TEXT = "Published tenders";

    @Override
    protected LocalDate getDefaultStartDate() {
        return OLDEST_NOTICE_DATE;
    }

    @Override
    protected String getFormTypeOptionText() {
        return FORM_TYPE_OPTION_TEXT;
    }
}
