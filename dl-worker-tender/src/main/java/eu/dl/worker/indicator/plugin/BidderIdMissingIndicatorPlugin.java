package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.MasterBid;
import eu.dl.dataaccess.dto.master.MasterBody;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;

import java.util.Objects;

/**
 * Missing bidder id.
 */
public class BidderIdMissingIndicatorPlugin extends LotIndicatorPlugin {
    @Override
    public final Indicator evaluate(final MasterTenderLot masterLot, final MasterTender tender) {
        if(tender == null || masterLot == null || masterLot.getBids() == null || masterLot.getBids().isEmpty()) {
            return insufficient();
        }

        MasterBid winningBid =
                masterLot.getBids().stream().filter(Objects::nonNull)
                        .filter(a -> a.getIsWinning() != null && a.getIsWinning()).findFirst().orElse(null);
        if(winningBid == null || winningBid.getBidders() == null || winningBid.getBidders().isEmpty()) {
            return insufficient();
        }

        for(MasterBody bidder: winningBid.getBidders()) {
            if(bidder.getId() == null || bidder.getId().isEmpty()) {
                return calculated(0d);
            }
        }

        return calculated(100d);
    }

    @Override
    public final String getType() {
        return TenderIndicatorType.TRANSPARENCY_BIDDER_ID_MISSING.name();
    }
}
