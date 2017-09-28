package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dto.indicator.BasicEntityRelatedIndicator;
import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.master.MasterTender;

import static eu.dl.dataaccess.dto.indicator.TenderIndicatorType.ADMINISTRATIVE_CENTRALIZED_PROCUREMENT;

/**
 * This plugin calculates Single bid indicator.
 */
public class CentralizedProcurementIndicatorPlugin implements IndicatorPlugin<MasterTender> {

    @Override
    public final Indicator evaulate(final MasterTender tender) {
        if (tender == null) {
            return null;
        }

        if (tender.getIsCentralProcurement() != null && tender.getIsCentralProcurement()) {
            Indicator indicator = new BasicEntityRelatedIndicator();
            indicator.setType(getType());
            return indicator;
        } else {
            return null;
        }
    }

    @Override
    public final String getType() {
        return ADMINISTRATIVE_CENTRALIZED_PROCUREMENT.name();
    }

}