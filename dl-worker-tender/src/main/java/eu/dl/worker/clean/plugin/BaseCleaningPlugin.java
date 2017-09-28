package eu.dl.worker.clean.plugin;

import eu.dl.dataaccess.dto.clean.Cleanable;
import eu.dl.dataaccess.dto.parsed.Parsable;
import eu.dl.worker.utils.BasePlugin;

/**
 * Provides base functionality(logging, config access) for all the cleaning
 * plugins.
 *
 * @param <T>
 *            parsable item
 * @param <U>
 *            cleanable item
 */
public abstract class BaseCleaningPlugin<T extends Parsable, U extends Cleanable> extends BasePlugin
    implements TenderCleaningPlugin<T, U> {
    
}
