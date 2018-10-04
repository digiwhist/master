package eu.datlab.worker.eu.raw;

import java.time.LocalDate;
import java.time.Month;
import java.time.temporal.ChronoUnit;

import eu.datlab.worker.raw.BaseDatlabIncrementalCrawler;

/**
 * Crawler for TED tender CVSs from 2006 to 2010.
 *
 * @author Tomas Mrazek
 */
public final class TedCSVTenderCrawler extends BaseDatlabIncrementalCrawler {

    private static final String VERSION = "1.0";

    private static final LocalDate DEFAULT_START_DATE = LocalDate.of(2006, Month.JANUARY, 1);

    /**
     * Last crawled year.
     */
    private static final int LAST_YEAR = 2010;

    /**
     * Contract notice CSV identifier.
     */
    private static final String NOTICE_IDENT = "CN";
    /**
     * Contract award notice CSV identifier.
     */
    private static final String AWARD_IDENT = "CAN";
    /**
     * CSV file url pattern.
     */
    private static final String CSV_URL_PATTERN =
        "http://data.europa.eu/euodp/repository/ec/dg-grow/mapps/TED_%s_%d.csv";


    @Override
    protected void crawlSourceForDate(final LocalDate date) {
        if (date.getYear() > LAST_YEAR) {
            logger.info("Crawling for date {} skiped. Date {} is after year {}.", date, LAST_YEAR);
            return;
        }

        // contract notices csv for year
        createAndPublishMessage(String.format(CSV_URL_PATTERN, NOTICE_IDENT, date.getYear()));

        // contract awards csv for year
        createAndPublishMessage(String.format(CSV_URL_PATTERN, AWARD_IDENT, date.getYear()));
    }

    @Override
    protected String getVersion() {
        return VERSION;
    }

    @Override
    protected LocalDate getDefaultStartDate() {
        return DEFAULT_START_DATE;
    }

    @Override
    protected ChronoUnit getIncrementUnit() {
        return ChronoUnit.YEARS;
    }
    
    @Override
    protected void initialSetup() {
    }

    @Override
    protected void finalCleanup() {
    }
}
