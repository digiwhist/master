package eu.dl.worker.clean.plugin;

import eu.dl.dataaccess.dto.clean.Cleanable;
import eu.dl.dataaccess.dto.parsed.Parsable;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;

/**
 * Provides base functionality for plugin that cleans date(time) fields. At least one DateTimeFormatter should be
 * defined for cleaning.
 *
 * @param <T>
 *      extending class (for fluent interface purposes)
 * @param <U>
 *      parsable item
 * @param <V>
 *      cleanable item
 *
 * @author Tomas Mrazek
 *
 */
public abstract class BaseDateTimePlugin<T extends BaseDateTimePlugin, U extends Parsable, V extends Cleanable>
    extends BaseCleaningPlugin<U, V> {
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
    public BaseDateTimePlugin(final List<DateTimeFormatter> formatters) {
        setFormatters(formatters);
    }

    /**
     * BaseDateTimePlugin should be initialised with the pattern of the date.
     *
     * @param formatter
     *       formatter used to parse the date/datetime fields
     */
    public BaseDateTimePlugin(final DateTimeFormatter formatter) {
        addFormatter(formatter);
    }

    /**
     * Sets list of formatters.
     *
     * @param formatter
     *      list of formatters
     * @return instance of {@code T} class for chaining
     */
    public final T setFormatters(final List<DateTimeFormatter> formatter) {
        this.formatters = formatter;
        return (T) this;
    }

    /**
     * Adds formatter used for cleaning.
     *
     * @param formatter
     *      formatter
     * @return instance of {@code T} class for chaining
     */
    public final T addFormatter(final DateTimeFormatter formatter) {
        if (!(this.formatters instanceof List)) {
            this.formatters = new ArrayList<>();
        }
        this.formatters.add(formatter);
        return (T) this;
    }
}
