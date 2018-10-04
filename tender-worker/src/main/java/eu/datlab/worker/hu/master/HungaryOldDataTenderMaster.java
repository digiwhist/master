package eu.datlab.worker.hu.master;

import java.util.List;

import eu.datlab.worker.master.BaseDatlabTenderMaster;
import eu.dl.dataaccess.dto.master.MasterBid;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import java.util.stream.Collectors;

/**
 * Masters tender data for old hungary data.
 */
public class HungaryOldDataTenderMaster extends BaseDatlabTenderMaster {

    private static final String VERSION = "1.0";

    @Override
    protected final void registerSpecificPlugins() {
    }

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final String getIncomingQueueName() {
        return getIncomingQueueNameFromConfig();
    }

    @Override
    protected final List<MatchedTender> sourceSpecificPreprocessData(final List<MatchedTender> items) {
        return items;
    }

    @Override
    protected final MasterTender sourceSpecificPostprocessData(final MasterTender item) {
        if (item.getLots() != null) {
            for (MasterTenderLot lot : item.getLots()) {
                if (lot.getBids() != null) {
                    List<MasterBid> winningBids = lot.getBids().stream().filter(b -> b.getIsWinning())
                        .collect(Collectors.toList());

                    if (winningBids.size() > 1 && winningBids.stream().anyMatch(b -> b.getIsConsortium())) {
                        lot.getBids().removeIf(b -> !b.getIsConsortium());
                    }
                }
            }
        }


        return item;
    }
}
