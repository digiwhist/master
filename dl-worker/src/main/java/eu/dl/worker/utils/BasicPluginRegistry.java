/**
 * 
 */
package eu.dl.worker.utils;

import java.util.LinkedHashMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Basic implementation of the plugin registry. Allows to define the order and
 * type of plugins used.
 * 
 * @param <T>
 *            Type of plugin to be used
 * 
 * @author Kuba Krafka
 */
public final class BasicPluginRegistry<T> implements PluginRegistry<T> {

	protected final Logger logger = LoggerFactory.getLogger(this.getClass().getName());
	
    private LinkedHashMap<String, T> plugins = new LinkedHashMap<String, T>();
	
	@Override
    public PluginRegistry registerPlugin(final String pluginId, final T plugin) {
		logger.debug("Registered new plugin with id {} and class {}", pluginId, plugin.getClass());
		plugins.put(pluginId, plugin);
		return this;
	}

	@Override
	public PluginRegistry unRegisterPlugin(final String pluginId) {
		plugins.remove(pluginId);
		return this;
	}

	@Override
    public LinkedHashMap<String, T> getPlugins() {
		return plugins;
	}

}
