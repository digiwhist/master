package eu.dl.worker.clean.plugin;

import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.utils.DateUtils;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;

/**
 * Plugin used to clean datetime fields.
 *
 * @author Tomas Mrazek
 *
 * @param <T>
 *      parsable item
 * @param <U>
 *      cleanable item
 */
public final class DateTimePlugin<T extends ParsedTender, U extends CleanTender>
    extends BaseDateTimePlugin<DateTimePlugin, T, U> {
    /**
     * DateTimePlugin should be initialised with the pattern of the date.
     *
     * @param formatter
     *            formatter used to parse the date/datetime fields
     */
    public DateTimePlugin(final DateTimeFormatter formatter) {
        super(formatter);
    }

    /**
     * DateTimePlugin should be initialised with the pattern of the date.
     *
     * @param formatters
     *            list of formatters used to parse the date/datetime fields
     */
    public DateTimePlugin(final List<DateTimeFormatter> formatters) {
        super(formatters);
    }

    /**
     * Cleans datetime fields.
     *
     * @param parsedTender
     *            tender with source data
     * @param cleanTender
     *            tender with clean data
     *
     * @return tender with cleaned data
     */
    @Override
    public U clean(final T parsedTender, final U cleanTender) {
        final LocalDateTime documentsDeadline =
            DateUtils.cleanDateTime(parsedTender.getDocumentsDeadline(), formatters);
        cleanTender.setDocumentsDeadline(documentsDeadline);
        logger.debug("Cleaned documentsDeadline for parsed tender {}, clean value \"{}\"", parsedTender.getId(),
            documentsDeadline);

        final LocalDateTime bidDeadline = DateUtils.cleanDateTime(parsedTender.getBidDeadline(), formatters);
        cleanTender.setBidDeadline(bidDeadline);
        logger.debug("Cleaned bidDeadline for parsed tender {}, clean value \"{}\"", parsedTender.getId(), bidDeadline);

        return cleanTender;
    }
}
