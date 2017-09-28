package eu.dl.worker.clean.plugin;

import eu.dl.dataaccess.dto.clean.Cleanable;
import eu.dl.dataaccess.dto.parsed.Parsable;
import java.util.List;
import java.util.Map;

/**
 * One of plugins used to clean codetable fields.
 *
 * @param <T>
 *            parsable item
 * @param <U>
 *            cleanable item
 *
 * @author Kuba Krafka
 */
public abstract class CodeTablePlugin<T extends Parsable, U extends Cleanable> extends BaseCleaningPlugin<T, U> {

    protected Map<Enum, List<String>> mapping;

    /**
     * CodeTable plugin should be initialised with mapping.
     *
     * @param mapping
     *            mapping of the values, the form should be like
     *            Enum.APPLE =>
     *              ("apples", "aples", "paple")
     *            Enum.MAPPLE =>
     *              ("mapples", "maple")
     */
    public CodeTablePlugin(final Map<Enum, List<String>> mapping) {
        this.mapping = mapping;
    }

    /**
     * Cleans codetable field. The proper mapping needs to be provided first.
     *
     * @param parsed
     *            parsed item with source data
     * @param clean
     *            clean item with clean data
     *
     * @return tender with cleaned data
     */
    @Override
    public abstract U clean(T parsed, U clean);

}
