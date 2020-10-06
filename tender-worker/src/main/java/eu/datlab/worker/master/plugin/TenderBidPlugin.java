package eu.datlab.worker.master.plugin;

import eu.dl.dataaccess.dto.master.MasterBid;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import eu.dl.dataaccess.dto.matched.MatchedBid;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.dataaccess.dto.matched.MatchedTenderLot;
import eu.dl.worker.master.plugin.BaseBidPlugin;
import eu.dl.worker.master.plugin.body.BodiesPlugin;
import eu.dl.worker.master.plugin.GeneralPricePlugin;
import eu.dl.worker.master.plugin.generic.LastPublishedPlugin;
import eu.dl.worker.master.plugin.generic.LogicalORPlugin;
import eu.dl.worker.master.plugin.generic.ModusPlugin;
import eu.dl.worker.master.plugin.generic.UnionPlugin;
import eu.dl.worker.master.plugin.generic.converter.TenderConverter;

import java.util.Arrays;

/**
 * Bid mastering plugin. Matches bids of matched tender lots and creates master record for each bid.
 */
public final class TenderBidPlugin extends BaseBidPlugin<MatchedTenderLot, MasterTenderLot, MatchedBid, MasterBid,
        MatchedTender> {

    @Override
    protected MasterBid createEmptyListItemInstance() {
        return new MasterBid();
    }

    @SuppressWarnings("unchecked")
    @Override
    protected void registerNestedMasterPlugins() {
        nestedPluginRegistry
                .registerPlugin("LOR", new LogicalORPlugin<>(Arrays.asList(
                        "isWinning", "isDisqualified", "wasInRequestedQuality", "wasFinishedOnTime",
                        "wasForEstimatedValue", "isSubcontracted", "isConsortium")))
                .registerPlugin("LNN", new LastPublishedPlugin<>(
                        Arrays.asList("disqualificationReason"), new TenderConverter()))
                .registerPlugin("Union", new UnionPlugin<>(Arrays.asList("unitPrices", "payments"),
                        new TenderConverter()))
                .registerPlugin("bodies", new BodiesPlugin(Arrays.asList("bidders", "subcontractors")))
                .registerPlugin("Documents", new TenderDocumentPlugin())
                .registerPlugin("MOD+LNN", new ModusPlugin<>(
                        Arrays.asList("subcontractedProportion"),
                        new TenderConverter()))
                .registerPlugin("Price", new GeneralPricePlugin<>(Arrays.asList("price", "subcontractedValue")));
    }
}
