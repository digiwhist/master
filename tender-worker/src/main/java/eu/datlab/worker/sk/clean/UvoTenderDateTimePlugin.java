package eu.datlab.worker.sk.clean;

import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.BaseCleaningPlugin;
import eu.dl.worker.clean.plugin.DateTimePlugin;

import static eu.datlab.worker.sk.clean.UvoTenderCleanerUtils.cleanDatesAndTimes;

import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.List;

/**
 * Tender clean dateTime plugin specific for SK Uvo.
 *
 * @author Michal Riha
 */
public class UvoTenderDateTimePlugin extends BaseCleaningPlugin<ParsedTender, CleanTender> {
    /**
     * Formatters used to parse the dates(time) fields.
     */
    protected List<DateTimeFormatter> formatters;

    /**
     * BaseDateTimePlugin should be initialised with the pattern of the date.
     *
     * @param formatters
     *       list of formatters used to parse the date(time) fields
     */
    public UvoTenderDateTimePlugin(final List<DateTimeFormatter> formatters) {
        this.formatters = formatters;
    }

    /**
     * BaseDateTimePlugin should be initialised with the pattern of the date.
     *
     * @param formatter
     *       formatter used to parse the date/datetime fields
     */
    public UvoTenderDateTimePlugin(final DateTimeFormatter formatter) {
        this.formatters = Arrays.asList(formatter);
    }


    @Override
    public final CleanTender clean(final ParsedTender parsedTender, final CleanTender cleanTender) {
        parsedTender.setBidDeadline(cleanDatesAndTimes(parsedTender.getBidDeadline()));
        parsedTender.setDocumentsDeadline(cleanDatesAndTimes(parsedTender.getDocumentsDeadline()));

        return new DateTimePlugin(formatters).clean(parsedTender, cleanTender);
    }


}
