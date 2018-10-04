package eu.dl.worker.raw;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.CrawlerAuditDAO;
import eu.dl.worker.Message;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;

/**
 * Base class for incremental crawlers. This class implements setting the crawling range and looping from given start
 * date to end date. Subclasses must implement how the crawling for each single date is performed and set the
 * incremental unit (day, year, etc.).
 */
public abstract class BaseIncrementalCrawler extends BaseCrawler {
    private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd");

    private final CrawlerAuditDAO crawlerAuditDao = getCrawlerAuditDAO();

    private LocalDate startDate;
    private LocalDate endDate;

    protected LocalDate actualDate;

    @Override
    public final void doWork(final Message message) {
        try {
            getTransactionUtils().begin();
            initialSetup();
            setCrawlingDateRange(message);
            logger.info("Crawler is searching from {} to {}.", startDate.toString(), endDate.toString());

            actualDate = startDate;
            while (!actualDate.isAfter(endDate)) {
                getTransactionUtils().begin();
                crawlSourceForDate(actualDate);
                crawlerAuditDao.updateLastCrawledDateForCrawler(actualDate);
                actualDate = actualDate.plus(1, getIncrementUnit());
                getTransactionUtils().commit();
            }
            finalCleanup();
        } catch (final Exception e) {
            logger.error("Crawling failed for date {} with exception {}", actualDate, e);
            throw new UnrecoverableException("Crawling failed.", e);
        }
    }

    /**
     * Initial custom setup if needed before the actual work is done.
     */
    protected abstract void initialSetup();

    /**
     * Does the actual work. Crawls the source for given date, extracts links and/or data and publishes messages for
     * downloader.
     *
     * @param date
     *         the date which the crawler should filter the source for
     */
    protected abstract void crawlSourceForDate(LocalDate date);

    /**
     * Cleaning after the work has been done. Might or might not be needed.
     */
    protected abstract void finalCleanup();

    /**
     * Gets default start date (date of the oldest record).
     *
     * @return default start date (date of the oldest record)
     */
    protected abstract LocalDate getDefaultStartDate();

    /**
     * Returns the unit to be used as an increment step (eg. DAYS, YEARS).
     *
     * @return the unit to be used as an increment step for incremental crawling
     */
    protected abstract ChronoUnit getIncrementUnit();

    /**
     * Sets date range for crawling. Dates passed in message take priority over dates stored in crawler audit log.
     *
     * @param message
     *         message that may contain start and end dates
     */
    private void setCrawlingDateRange(final Message message) {
        final LocalDate yesterday = LocalDate.now().minusDays(1);

        // set the start date
        final String startDateString = message.getValue("startDate");
        if (startDateString != null) {
            // set dates from message
            startDate = LocalDate.parse(startDateString, DATE_FORMATTER);
            if (startDate.isAfter(yesterday)) {
                logger.error("Crawler \"startDate\" date {} cannot be after yesterday ({})", startDate, yesterday);
                throw new UnrecoverableException("Crawling failed. Start date is after yesterday.");
            } else if (startDate.isBefore(getDefaultStartDate())) {
                startDate = getDefaultStartDate();
                logger.warn("\"startDate\" date is before default start date. Setting it to the default start date.");
            }

            final String endDateString = message.getValue("endDate");
            if (endDateString != null) {
                endDate = LocalDate.parse(endDateString, DATE_FORMATTER);
                if (endDate.isAfter(yesterday)) {
                    endDate = yesterday;
                    logger.warn("\"endDate\" is today or later. Setting it to yesterday.");
                }
            } else {
                endDate = yesterday;
            }

            if (startDate.isAfter(endDate)) {
                logger.error("Crawler start date {} cannot be after end date {}", startDate, endDate);
                throw new UnrecoverableException("Crawling failed. Start date is after end date.");
            }

            // appends information about the crawled date range to the common crawler audit identifier
            crawlerAuditDao.setWorkerName(new StringBuilder(getName())
                .append("-").append(startDate.format(DateTimeFormatter.BASIC_ISO_DATE))
                .append("-").append(endDate.format(DateTimeFormatter.BASIC_ISO_DATE))
                .toString());
        } else {
            // set dates from crawler audit collection
            final LocalDate lastCrawledDate = crawlerAuditDao.getLastCrawledDateByCrawler();
            if (lastCrawledDate != null) {
                startDate = lastCrawledDate.plus(1, getIncrementUnit());
            } else {
                startDate = getDefaultStartDate();
            }
            endDate = yesterday;
        }
    }

    /**
     * @return start date
     */
    public final LocalDate getStartDate() {
        return startDate;
    }
    /**
     * @return end date
     */
    public final LocalDate getEndDate() {
        return endDate;
    }
    
    /**
     * Provides DAO to used to store progress of crawling.
     * 
     * @return crawler audit DAO
     */
    protected abstract CrawlerAuditDAO getCrawlerAuditDAO();
}
