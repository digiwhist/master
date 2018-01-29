package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dto.indicator.Indicator;

/**
 * Interface for the indicator plugin. The calculation of indicator happens in
 * the plugin itself.
 *
 * @param <T>
 *            evaulated item
 */
public interface IndicatorPlugin<T> {

    /**
     * Takes data from source matched items, masters some fields of them and
     * returns result.
     *
     * @param item
     *            item to be evaulated
     * 
     * @return calculated indicator or null if none
     */
    Indicator evaluate(T item);

    /**
     * Indicator type calculated by this plugin.
     *
     * @return indicator type calculated by this plugin
     */
    String getType();
}
