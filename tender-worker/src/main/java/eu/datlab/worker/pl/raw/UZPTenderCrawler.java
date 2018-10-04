package eu.datlab.worker.pl.raw;

import java.time.LocalDate;
import java.time.Month;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.HashMap;

import eu.datlab.worker.raw.BaseDatlabIncrementalFtpCrawler;

/**
 * Finds all daily packages on the TED ftp.
 *
 * @author Tomas Mrazek
 */
public final class UZPTenderCrawler extends BaseDatlabIncrementalFtpCrawler {
    /**
     * Directory that contains xml package archives.
     */
    private static final String XML_PACKAGES_DIR = "bzp/xml/";
    private static final String XML_PACKAGES_DIR_2017 = "bzp2/xml/";

    private static final DateTimeFormatter FILE_NAME_DATE_FORMATTER = DateTimeFormatter.ofPattern("yyyyMMdd");
    private static final DateTimeFormatter DIR2007_NAME_DATE_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd");
    private static final LocalDate DEFAULT_START_DATE = LocalDate.of(2007, Month.AUGUST, 1);
    private static final String VERSION = "4";
    private static final LocalDate DAILY_PACKAGES_START_DATE = LocalDate.of(2009, Month.JANUARY, 1);

    @Override
    protected void crawlSourceForDate(final LocalDate date) {
        final int year = date.getYear();
        final String packageDir = (year < 2017 ? XML_PACKAGES_DIR : XML_PACKAGES_DIR_2017) + year + "/";
        final String regex = String.format("^%s\\.exe", date.format(FILE_NAME_DATE_FORMATTER));
        //for year 2007 and 2008 is provided year package only. For others dates provides daily package.
        if (date.isBefore(DAILY_PACKAGES_START_DATE)) {
            final  HashMap<String, Object> metaData = new HashMap<>();
            String filterRegex = null;

            //extract only files for given date
            //2007_xml.rar - file = files in dir 2007-MM-dd/
            //2008_xml.rar - file = 2008MMdd.exe
            if (year == 2007) {
                filterRegex = "^" + date.format(DIR2007_NAME_DATE_FORMATTER) + ".*";
            } else if (year == 2008) {
                filterRegex = regex;
            }
            metaData.put("regex", filterRegex);

            //RAR archive cache controll. Archive is removed after processing the last day of crawled date interval.
            metaData.put("cache", date.isBefore(getEndDate()));

            createAndPublishMessage(String.format("%1$s%2$d_xml.rar", packageDir, year), metaData);
        } else {
            searchFiles(packageDir, regex);
        }
    }

    @Override
    protected LocalDate getDefaultStartDate() {
        return DEFAULT_START_DATE;
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
