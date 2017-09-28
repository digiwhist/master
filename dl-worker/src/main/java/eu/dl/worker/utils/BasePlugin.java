package eu.dl.worker.utils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import eu.dl.core.config.Config;

/**
 * Base plugin providing functionality such as logs etc.
 *
 */
public abstract class BasePlugin {
    protected final Logger logger = LoggerFactory.getLogger(this.getClass().getName());

    protected final Config config = Config.getInstance();
}
