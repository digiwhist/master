package eu.digiwhist.worker.sk.clean;

import static eu.digiwhist.worker.sk.clean.UvoTenderCleanerUtils.cleanDatesAndTimes;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.BaseCleaningPlugin;
import eu.dl.worker.clean.plugin.DatePlugin;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.List;

/**
 * Tender clean date plugin specific for SK Uvo.
 *
 * @author Michal Riha
 */
public class UvoTenderDatePlugin extends BaseCleaningPlugin<ParsedTender, CleanTender> {
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
    public UvoTenderDatePlugin(final List<DateTimeFormatter> formatters) {
        this.formatters = formatters;
    }

    /**
     * BaseDateTimePlugin should be initialised with the pattern of the date.
     *
     * @param formatter
     *       formatter used to parse the date/datetime fields
     */
    public UvoTenderDatePlugin(final DateTimeFormatter formatter) {
        this.formatters = Arrays.asList(formatter);
    }


    @Override
    public final CleanTender clean(final ParsedTender parsedTender, final CleanTender cleanTender) {
        parsedTender.setAwardDeadline(cleanDatesAndTimes(parsedTender.getAwardDeadline()));
        parsedTender.setEnquiryDeadline(cleanDatesAndTimes(parsedTender.getEnquiryDeadline()));

        return new DatePlugin(formatters).clean(parsedTender, cleanTender);
    }
}
