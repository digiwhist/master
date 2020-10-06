package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.MasterBody;
import eu.dl.dataaccess.dto.master.MasterTender;

/**
 * Missing buyer name.
 */
public class BuyerNameMissingIndicatorPlugin extends BaseIndicatorPlugin
        implements IndicatorPlugin<MasterTender> {

    @Override
    public final Indicator evaluate(final MasterTender tender) {
        if(tender == null || tender.getBuyers() == null || tender.getBuyers().isEmpty()) {
            return insufficient();
        }
        for(MasterBody buyer: tender.getBuyers()) {
            if(buyer.getName() == null || buyer.getName().isEmpty()) {
                return calculated(0d);
            }
        }
        return calculated(100d);
    }

    @Override
    public final String getType() {
        return TenderIndicatorType.TRANSPARENCY_BUYER_NAME_MISSING.name();
    }
}
