package eu.dl.worker.clean.plugin;

import eu.dl.dataaccess.dto.clean.Cleanable;
import eu.dl.dataaccess.dto.parsed.Parsable;
import java.text.NumberFormat;
import java.util.Arrays;
import java.util.List;

/**
 * Provides base functionality for plugin that cleans number fields. At least one NumberFormat should be
 * defined for cleaning.
 *
 * @author Tomas Mrazek
 *
 * @param <T>
 *      parsable item
 * @param <U>
 *      cleanable item
 */
public abstract class BaseNumberPlugin<T extends Parsable, U extends Cleanable> extends BaseCleaningPlugin<T, U> {

    protected List<NumberFormat> formats;

    /**
     * BaseNumberPlugin initialization by the number format.
     *
     * @param format
     *            number format
     */
    public BaseNumberPlugin(final NumberFormat format) {
        this.formats = Arrays.asList(format);
    }

    /**
     * BaseNumberPlugin initialization by the number formats.
     *
     * @param formats
     *            list of number formats
     */
    public BaseNumberPlugin(final List<NumberFormat> formats) {
        this.formats = formats;
    }

    /**
     * Cleans number fields.
     *
     * @param parsed
     *          parsable item
     * @param clean
     *          cleanable item
     *
     * @return cleanable item with cleaned data
     */
    @Override
    public abstract U clean(T parsed, U clean);
}
