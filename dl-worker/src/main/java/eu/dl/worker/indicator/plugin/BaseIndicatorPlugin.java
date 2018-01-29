package eu.dl.worker.indicator.plugin;

import java.util.HashMap;

import eu.dl.dataaccess.dto.indicator.BasicIndicator;
import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.indicator.IndicatorStatus;

/**
 * Basic class for indicator plugins. Provides common functionality to work
 * with indicators.
 * @author Jakub Krafka
 *
 */
public abstract class BaseIndicatorPlugin {
	/**
	 * Returns properly initialized indicator with undefined status.
	 *
	 * @return indicator
	 */
	protected final Indicator undefined() {
		Indicator indicator = new BasicIndicator();
        indicator.setType(getType());
        indicator.setStatus(IndicatorStatus.UNDEFINED);
        return indicator;
	}
	
	/**
	 * Returns properly initialized indicator with insufficient data status.
	 * 
	 * @return indicator
	 */
	protected final Indicator insufficient() {
		return insufficient(null);
	}
	
	/**
	 * Returns properly initialized indicator with insufficient data status.
	 * 
	 * @param metaData meta data
	 * 
	 * @return indicator
	 */
	protected final Indicator insufficient(final HashMap<String, Object> metaData) {
		Indicator indicator = new BasicIndicator();
        indicator.setType(getType());
        indicator.setStatus(IndicatorStatus.INSUFFICIENT_DATA);
        indicator.setMetaData(metaData);
        return indicator;
	}
	
	
	/**
	 * Returns properly initialized indicator with calculated data status and value.
	 * 
	 * @param value value of the indicator
	 * 
	 * @return indicator
	 */
	protected final Indicator calculated(final Double value) {
		return calculated(value, null);
	}
	
	/**
	 * Returns properly initialized indicator with insufficient data status.
	 * 
	 * @param value value of the indicator
	 * @param metaData meta data
	 * 
	 * @return indicator
	 */
	protected final Indicator calculated(final Double value, final HashMap<String, Object> metaData) {
		Indicator indicator = new BasicIndicator();
		indicator.setValue(value);
        indicator.setType(getType());
        indicator.setStatus(IndicatorStatus.CALCULATED);
        indicator.setMetaData(metaData);
        return indicator;
	}
	
	/**
     * Indicator type calculated by this plugin.
     *
     * @return indicator type calculated by this plugin
     */
	public abstract String getType();
}
