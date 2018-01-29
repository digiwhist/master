package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dto.indicator.BasicIndicator;
import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.master.MasterTender;

import static eu.dl.dataaccess.dto.indicator.TenderIndicatorType.ADMINISTRATIVE_CENTRALIZED_PROCUREMENT;

/**
 * This plugin calculates Single bid indicator.
 */
public class CentralizedProcurementIndicatorPlugin extends BaseIndicatorPlugin
        implements IndicatorPlugin<MasterTender> {

    @Override
    public final Indicator evaluate(final MasterTender tender) {
        if (tender == null) {
            return null;
        }

        Indicator indicator = new BasicIndicator();
        indicator.setType(getType());

        if (tender.getIsCentralProcurement() != null && tender.getIsCentralProcurement()) {
            return calculated(100d);
        } else if (tender.getIsCentralProcurement() != null && !tender.getIsCentralProcurement()) {
            return calculated(0d);
        } else {
            return insufficient();
        }
    }

    @Override
    public final String getType() {
        return ADMINISTRATIVE_CENTRALIZED_PROCUREMENT.name();
    }

}