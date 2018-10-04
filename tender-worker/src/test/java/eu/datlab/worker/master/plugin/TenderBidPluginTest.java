package eu.datlab.worker.master.plugin;

import eu.dl.dataaccess.dto.master.MasterTenderLot;
import eu.dl.dataaccess.dto.matched.MatchedBid;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.dataaccess.dto.matched.MatchedTenderLot;
import eu.dl.dataaccess.dto.matched.StructuredLotId;
import eu.dl.worker.master.plugin.BaseBidPlugin;
import org.junit.Test;

import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertTrue;

/**
 * Test of bid matching and mastering.
 *
 * @author Marek Mikes
 */
public final class TenderBidPluginTest {
    private static final String TENDER1_ID = "tender1ID";
    private static final String TENDER2_ID = "tender2ID";
    private static final String TENDER3_ID = "tender3ID";
    private static final String TENDER4_ID = "tender4ID";

    private static final String BIDDER_GROUP_ID_1 = "bidderGroupID1";
    private static final String BIDDER_GROUP_ID_2 = "bidderGroupID2";
    private static final String BIDDER_GROUP_ID_3 = "bidderGroupID3";

    private final MatchedTender tender1 = new MatchedTender()
            .addLot(new MatchedTenderLot()
                    .setStructuredId(new StructuredLotId()
                            .setTenderId(TENDER1_ID))
                    .addBid(new MatchedBid()
                            .addBidder(new MatchedBody()
                                    .setGroupId(BIDDER_GROUP_ID_1)))
                    .addBid(new MatchedBid()
                            .addBidder(new MatchedBody()
                                    .setGroupId(BIDDER_GROUP_ID_2)))
                    .addBid(new MatchedBid()
                            .addBidder(new MatchedBody()
                                    .setGroupId(BIDDER_GROUP_ID_3))));
    private final MatchedTender tender2 = new MatchedTender()
            .addLot(new MatchedTenderLot()
                    .setStructuredId(new StructuredLotId()
                            .setTenderId(TENDER2_ID))
                    .addBid(new MatchedBid()
                            .addBidder(new MatchedBody()
                                    .setGroupId(BIDDER_GROUP_ID_3)))
                    .addBid(new MatchedBid()
                            .addBidder(new MatchedBody()
                                    .setGroupId(BIDDER_GROUP_ID_2)))
                    .addBid(new MatchedBid()
                            .addBidder(new MatchedBody()
                                    .setGroupId(BIDDER_GROUP_ID_1))));
    private final MatchedTender tender3 = new MatchedTender()
            .addLot(new MatchedTenderLot()
                    .setStructuredId(new StructuredLotId()
                            .setTenderId(TENDER3_ID))
                    .addBid(new MatchedBid()
                            .addBidder(new MatchedBody()
                                    .setGroupId(BIDDER_GROUP_ID_1)))
                    .addBid(new MatchedBid()
                            .addBidder(new MatchedBody()
                                    .setGroupId(BIDDER_GROUP_ID_2))));
    private final MatchedTender tender4 = new MatchedTender()
            .addLot(new MatchedTenderLot()
                    .setStructuredId(new StructuredLotId()
                            .setTenderId(TENDER4_ID))
                    .addBid(new MatchedBid()
                            .addBidder(new MatchedBody()
                                    .setGroupId(BIDDER_GROUP_ID_1))));

    /**
     * Default constructor to set tender IDs. The setter does not support fluent interface, so we call it here.
     */
    public TenderBidPluginTest() {
        tender1.setId(TENDER1_ID);
        tender2.setId(TENDER2_ID);
        tender3.setId(TENDER3_ID);
        tender4.setId(TENDER4_ID);
    }

    /**
     * Test of master bid count.
     */
    @Test
    public void masterBidCountTest() {
        final List<MatchedTender> tenders = Arrays.asList(tender1, tender2, tender3, tender4);

        final BaseBidPlugin bidPlugin = new TenderBidPlugin();
        MasterTenderLot masterTenderLot = new MasterTenderLot();
        bidPlugin.master(Arrays.asList(tender1.getLots().get(0), tender2.getLots().get(0), tender3.getLots().get(0),
                tender4.getLots().get(0)),
                masterTenderLot,
                Arrays.asList(tender1.getLots().get(0), tender2.getLots().get(0), tender3.getLots().get(0),
                        tender4.getLots().get(0)));

        // master lot should have three bids (it is mastered from three bid groups according to bidder group IDs)
        assertTrue(masterTenderLot.getBids().size() == 3);
    }
}
