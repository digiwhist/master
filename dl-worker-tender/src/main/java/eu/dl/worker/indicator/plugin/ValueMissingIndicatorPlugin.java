package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.MasterBid;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;

import java.util.Objects;

/**
 * Missing bid price.
 */
public class ValueMissingIndicatorPlugin extends LotIndicatorPlugin {
    @Override
    public final Indicator evaluate(final MasterTenderLot masterLot, final MasterTender tender) {
        if (tender == null || masterLot == null) {
            return insufficient();
        }

        if(masterLot.getBids() == null || masterLot.getBids().isEmpty()) {
            return calculated(0d);
        }

        MasterBid winningBid =
                masterLot.getBids().stream().filter(Objects::nonNull).filter(a -> a.getIsWinning() != null
                        && a.getIsWinning()).findFirst().orElse(null);
        if (winningBid == null) {
            return calculated(0d);
        }

        return (winningBid.getPrice() == null
                || (winningBid.getPrice().getNetAmountNational() == null && winningBid.getPrice().getNetAmount() == null))
                ? calculated(0d)
                : calculated(100d);
    }

    @Override
    public final String getType() {
        return TenderIndicatorType.TRANSPARENCY_VALUE_MISSING.name();
    }
}
