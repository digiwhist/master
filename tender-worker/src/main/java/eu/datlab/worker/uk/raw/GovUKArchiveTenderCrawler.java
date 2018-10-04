package eu.datlab.worker.uk.raw;

import java.time.LocalDate;
import java.time.Month;
import java.time.format.DateTimeFormatter;

import eu.dl.dataaccess.dao.DummyTransactionUtils;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.worker.Message;
import eu.dl.worker.raw.BaseCrawler;

/**
 * Generates urls for tenders archive data downloading. They are generated as monthly packages from date range
 * beginning on January 2011 and ending January 2015.
 *
 * @author Tomas Mrazek
 */
public final class GovUKArchiveTenderCrawler extends BaseCrawler {
    private static final String VERSION = "1";
    /**
     * Tender xml url patern.
     */
    private static final String URL_ARCHIVE_PATTERN = "https://data.gov"
            + ".uk/data/contracts-finder-archive/static/files/" + "notices_%s.xml";
    /**
     * Beginning date of archive.
     */
    private static final LocalDate ARCHIVE_FROM = LocalDate.of(2011, Month.JANUARY, 1);
    /**
     * Ending date of archive.
     */
    private static final LocalDate ARCHIVE_TO = LocalDate.of(2015, Month.JANUARY, 1);
    /**
     * Date format for url parameter.
     */
    private static final DateTimeFormatter URL_DATE_FORMATTER = DateTimeFormatter.ofPattern("yyyy_MM");

    @Override
    protected void doWork(final Message message) {
        LocalDate actual = ARCHIVE_FROM;

        while (!actual.isAfter(ARCHIVE_TO)) {
            final String url = String.format(URL_ARCHIVE_PATTERN, actual.format(URL_DATE_FORMATTER));
            logger.info("Url generated for {}", actual.format(DateTimeFormatter.ISO_DATE));
            createAndPublishMessage(url);
            actual = actual.plusMonths(1);
        }
    }

    @Override
    protected String getVersion() {
        return VERSION;
    }

    @Override
    protected TransactionUtils getTransactionUtils() {
        return new DummyTransactionUtils();
    }
}
