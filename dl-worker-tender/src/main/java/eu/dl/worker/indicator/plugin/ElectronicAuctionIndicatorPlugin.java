package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dto.indicator.BasicIndicator;
import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;

/**
 * This plugin calculates electronic aution indicator.
 */
public class ElectronicAuctionIndicatorPlugin extends BaseIndicatorPlugin
        implements IndicatorPlugin<MasterTender> {

    @Override
    public final Indicator evaluate(final MasterTender tender) {
        if (tender == null) {
            return null;
        }

        Indicator indicator = new BasicIndicator();
        indicator.setType(getType());

        if (tender.getIsElectronicAuction() != null && tender.getIsElectronicAuction()) {
            return calculated(100d);
        } else {
            if (tender.getLots() != null) {
                // iterate over lots and try to identify those, which are covered by GPA
                for (MasterTenderLot lot : tender.getLots()) {
                    if (lot.getIsElectronicAuction() != null && lot.getIsElectronicAuction()) {
                        return calculated(100d);
                    }
                }
            }
        }

        if (tender.getIsElectronicAuction() != null && !tender.getIsElectronicAuction()) {
            return calculated(0d);
        } else {
            return insufficient();
        }
    }

    @Override
    public final String getType() {
        return TenderIndicatorType.ADMINISTRATIVE_ELECTRONIC_AUCTION.name();
    }

}
