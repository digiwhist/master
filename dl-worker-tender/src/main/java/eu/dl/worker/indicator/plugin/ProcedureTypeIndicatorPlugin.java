package eu.dl.worker.indicator.plugin;

import eu.dl.core.config.Config;
import eu.dl.dataaccess.dto.indicator.BasicEntityRelatedIndicator;
import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.MasterTender;

/**
 * This plugin calculates procedure type indicator.
 */
public class ProcedureTypeIndicatorPlugin implements IndicatorPlugin<MasterTender> {

	/**
     * Application config instance.
     */
    private static final Config config = Config.getInstance();
    
    @Override
    public final Indicator evaulate(final MasterTender tender) {
        if (tender == null || tender.getCountry() == null) {
            return null;
        }

        String configKey = "indicator." + tender.getCountry();
        
        if (tender.getProcedureType() == null || tender.getProcedureType().toString().isEmpty()) {
        	configKey = configKey + ".procedureType.missing";
        } else {
        	configKey = configKey + ".procedureType." + tender.getProcedureType();        	
        }
        
        if (config.getParam(configKey) != null && config.getParam(configKey).equals("YES")) {
	        Indicator indicator = new BasicEntityRelatedIndicator();
	        indicator.setType(getType());
	        return indicator;
        } 
        
        return null;
    }

    @Override
    public final String getType() {
        return TenderIndicatorType.CORRUPTION_PROCEDURE_TYPE.name();
    }

}
