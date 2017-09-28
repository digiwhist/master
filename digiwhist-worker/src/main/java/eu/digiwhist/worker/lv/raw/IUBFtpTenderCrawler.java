package eu.digiwhist.worker.lv.raw;

import eu.digiwhist.worker.raw.BaseDigiwhistIncrementalFtpCrawler;
import eu.dl.core.UnrecoverableException;
import java.io.IOException;
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import org.apache.commons.net.ftp.FTPFile;

/**
 * Ftp tender crawler for IUB.
 *
 * @author Michal Riha
 */
public final class IUBFtpTenderCrawler extends BaseDigiwhistIncrementalFtpCrawler {
    private static final String VERSION = "2";
    private static final LocalDate FIRST_DATE_AVAILABLE = LocalDate.of(2013, 1, 1);

    

    @Override
    protected void crawlSourceForDate(final LocalDate date) {
        try {
            getFtpClient().setSoTimeout(30000);
            logger.info("Crawling for date {}", date);
            publishBulkForDate(date);
        } catch (IOException e) {
            logger.error("Crawling failed with exception {}", e);
            throw new UnrecoverableException("Unable to crawl page", e);
        }
    }

    /**
     * Attempts to find bulk for the given {@code date} and sends link to RabbitMQ queue.
     *
     * @param date
     *         date for filtering the search results for
     * @throws IOException
     *         throws IOException when problem with ftp connection
     */
    private void publishBulkForDate(final LocalDate date) throws IOException {
        // Bulk dir pattern 'year/month_year'
        String path = String.format("%1$d/%2$02d_%1$d", date.getYear(), date.getMonthValue());
        // Bulk file name pattern 'day_month_year.tar.gz'
        String fileName = String.format("%3$02d_%2$02d_%1$d.tar.gz", date.getYear(), date.getMonthValue(),
            date.getDayOfMonth());

        logger.debug("Crawling folder: {}", path);
        final FTPFile[] files = getFtpClient().listFiles(path);
        for (final FTPFile file : files) {
            if (file.getName().equals(fileName)) {
                createAndPublishMessage(
                    String.format("ftp://%s/%s/%s", getFtpClient().getRemoteAddress().getHostName(), path, fileName));
                
                return;
            }
        }
        
        logger.debug("File for {} not found", date);
    }

    @Override
    protected LocalDate getDefaultStartDate() {
        return FIRST_DATE_AVAILABLE;
    }

    @Override
    protected String getVersion() {
        return VERSION;
    }

    @Override
    protected ChronoUnit getIncrementUnit() {
        return ChronoUnit.DAYS;
    }

    @Override
    protected void sourceSpecificInitialFtpSetup() {
    }

    @Override
    protected void sourceSpecificFinalFtpCleanup() {
    }
}
