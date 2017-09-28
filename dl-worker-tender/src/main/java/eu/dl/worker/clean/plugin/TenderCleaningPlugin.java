package eu.dl.worker.clean.plugin;

import eu.dl.dataaccess.dto.clean.Cleanable;
import eu.dl.dataaccess.dto.parsed.Parsable;

/**
 * Interface for the cleaning plugin. The cleaning happens in the clean action.
 *
 * @author Kuba Krafka
 *
 * @param <T>
 *            parsable item
 * @param <U>
 *            cleanable item
 */
public interface TenderCleaningPlugin<T extends Parsable, U extends Cleanable> extends CleaningPlugin<T, U> {

}
