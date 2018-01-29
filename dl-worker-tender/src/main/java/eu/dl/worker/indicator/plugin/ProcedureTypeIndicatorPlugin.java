package eu.dl.worker.indicator.plugin;

import eu.dl.core.config.Config;
import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.MasterTender;

import java.util.HashMap;

/**
 * This plugin calculates procedure type indicator.
 */
public class ProcedureTypeIndicatorPlugin extends BaseIndicatorPlugin implements IndicatorPlugin<MasterTender> {

	/**
     * Application config instance.
     */
    private static final Config config = Config.getInstance();
    
    @Override
    public final Indicator evaluate(final MasterTender tender) {
        if (tender == null || tender.getCountry() == null) {
            return insufficient();
        }

        String configKey = "indicator." + tender.getCountry();
        HashMap<String, Object> metaData = null;
        if (tender.getProcedureType() == null || tender.getProcedureType().toString().isEmpty()) {
        	configKey = configKey + ".procedureType.missing";
        } else {
            metaData = new HashMap<>();
            metaData.put("procedureType", tender.getProcedureType().toString());
        	configKey = configKey + ".procedureType." + tender.getProcedureType();
        }

        if (config.getParam(configKey) != null && config.getParam(configKey).equals("YES")) {
            return calculated(0d, metaData);
        } else {
            return calculated(100d, metaData);
        }
    }

    @Override
    public final String getType() {
        return TenderIndicatorType.INTEGRITY_PROCEDURE_TYPE.name();
    }

}
