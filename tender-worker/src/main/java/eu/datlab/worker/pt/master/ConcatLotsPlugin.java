package eu.datlab.worker.pt.master;

import eu.dl.dataaccess.dto.master.MasterBid;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.dataaccess.dto.matched.StructuredBidId;
import eu.dl.dataaccess.dto.matched.StructuredLotId;
import eu.dl.dataaccess.dto.utils.InitUtils;
import eu.dl.worker.master.plugin.MasterPlugin;
import eu.dl.worker.utils.BasePlugin;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * This plugin concatenates values from multiple arrays.
 *
 */
public final class ConcatLotsPlugin
        extends BasePlugin implements MasterPlugin<MatchedTender, MasterTender, MatchedTender> {


    /**
     * Initializes the plugin with field names to be mastered. The plugin will
     * call getFieldName and setFieldName methods on items.
     *
     */
    public ConcatLotsPlugin() {
        super();
    }


    @Override
    public MasterTender master(final List<MatchedTender> items, final MasterTender finalItem, final List<MatchedTender> context) {

        // collecting all lots
        List<MasterTenderLot> lotsMaster = new ArrayList<>();
        for (MatchedTender tender : items) {
            if (tender.getLots() != null) {
                List<MasterTenderLot> lotsMasterPartial = InitUtils.matchedToMasterLot(tender.getLots());
                for (int i = 0; i < lotsMasterPartial.size(); i++) {
                    MasterTenderLot lot = lotsMasterPartial.get(i);

                    // setting source lot id
                    lot.setSourceLotIds(Arrays.asList(
                            new StructuredLotId()
                                .setLotId(Integer.toString(i + 1))
                                .setTenderId(tender.getTenderId())
                    ));

                    for (int j = 0; j < lot.getBids().size(); j++) {
                        MasterBid bid = lot.getBids().get(j);

                        // setting source bid id
                        bid.setSourceBidIds(Arrays.asList(
                                new StructuredBidId()
                                        .setBidId(Integer.toString(j + 1))
                                        .setLotId(Integer.toString(i + 1))
                                        .setTenderId(tender.getTenderId())
                        ));
                    }

                    lotsMaster.add(lot);
                }

            }
        }

        // setting mastered lots
        if (lotsMaster.size() > 0) {
            finalItem.setLots(lotsMaster);
            finalItem.setHasLots(true);
        }

        return finalItem;
    }
}
