package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dto.indicator.BasicEntityRelatedIndicator;
import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;

/**
 * This plugin calculates framework agreement indicator.
 */
public class FrameworkAgreementIndicatorPlugin implements IndicatorPlugin<MasterTender> {

    @Override
    public final Indicator evaulate(final MasterTender tender) {
        if (tender == null) {
            return null;
        }

        Indicator indicator = new BasicEntityRelatedIndicator();
        indicator.setType(getType());

        if (tender.getIsFrameworkAgreement() != null && tender.getIsFrameworkAgreement()) {
            return indicator;
        } else if (tender.getLots() != null) {
            // iterate over lots and try to identify those, which are covered by GPA
            for (MasterTenderLot lot : tender.getLots()) {
                if (lot.getIsFrameworkAgreement() != null && lot.getIsFrameworkAgreement()) {
                    return indicator;
                }
            }
        }

        return null;
    }

    @Override
    public final String getType() {
        return TenderIndicatorType.ADMINISTRATIVE_FRAMEWORK_AGREEMENT.name();
    }

}
