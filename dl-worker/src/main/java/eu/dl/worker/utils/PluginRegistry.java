package eu.dl.worker.utils;

import java.util.LinkedHashMap;

/**
 * Plugins are being registered here. Please notice, that the order
 * matters(plugins will be processed in the FIFO way).
 * 
 * @param <T>
 *            Type of plugin to be used
 * 
 * @author Kuba Krafka
 * 
 */
public interface PluginRegistry<T> {
    /**
     * Registers plugin under the unique id. If there is the same id used for
     * the second time, the ids is replaced.
     * 
     * @param pluginId
     *            unique id of the plugin
     * @param plugin
     *            plugin instance
     * 
     * @return self for the fluent interfaces
     */
    PluginRegistry<T> registerPlugin(String pluginId, T plugin);
	
    /**
     * Unregisters plugin from the queue.
     * 
     * @param pluginId
     *            unique id of the plugin
     * 
     * @return self for the fluent interface
     */
	PluginRegistry<T> unRegisterPlugin(String pluginId);
	
    /**
     * Return the whole queue of plugins. The plugins will be exacuted in the
     * FIFO order.
     * 
     * @return plugin queue
     */
    LinkedHashMap<String, T> getPlugins();
}
