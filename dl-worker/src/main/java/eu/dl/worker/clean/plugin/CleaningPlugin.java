package eu.dl.worker.clean.plugin;

import eu.dl.dataaccess.dto.clean.Cleanable;
import eu.dl.dataaccess.dto.parsed.Parsable;

/**
 * Interface for the cleaning plugin. The cleaning happens in the clean action.
 *
 * @param <T>
 *            parsable item
 * @param <V>
 *            cleanable item
 */
public interface CleaningPlugin<T extends Parsable, V extends Cleanable> {
    /**
     * Takes data from source parsed tender, cleans them and stores result into
     * clean tender.
     *
     * @param parsedItem
     *            source tender being cleaned
     * @param cleanItem
     *            clean item with some parts already cleaned
     *
     * @return cleaned item
     */
    V clean(T parsedItem, V cleanItem);
}