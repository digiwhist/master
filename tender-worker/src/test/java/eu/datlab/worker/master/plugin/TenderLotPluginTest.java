package eu.datlab.worker.master.plugin;

import eu.dl.dataaccess.dto.codetables.SelectionMethod;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.matched.MatchedBid;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.dataaccess.dto.matched.MatchedTenderLot;
import eu.dl.dataaccess.dto.matched.StructuredLotId;
import eu.dl.worker.master.plugin.BaseTenderLotPlugin;
import eu.dl.worker.master.plugin.specific.FundingsPlugin;
import org.junit.Test;

import java.time.LocalDate;
import java.util.Arrays;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

/**
 * Test of tender lot matching and mastering.
 *
 * @author Marek Mikes
 */
public final class TenderLotPluginTest {
    private static final String TENDER1_ID = "tender1ID";
    private static final String TENDER2_ID = "tender2ID";
    private static final String TENDER3_ID = "tender3ID";

    private static final String BIDDER_GROUP_ID_1 = "bidderGroupID1";
    private static final String BIDDER_GROUP_ID_2 = "bidderGroupID2";
    private static final String BIDDER_GROUP_ID_3 = "bidderGroupID3";
    private static final String BIDDER_GROUP_ID_4 = "bidderGroupID4";

    private final LocalDate correctContractSignatureDate = LocalDate.now().plusYears(2);
    private final LocalDate incorrectContractSignatureDate = LocalDate.now().minusYears(2);

    private final MatchedTender tender1 = new MatchedTender()
            .addLot(new MatchedTenderLot()
                    .setStructuredId(new StructuredLotId()
                            .setTenderId(TENDER1_ID))
                    .setSelectionMethod(SelectionMethod.LOWEST_PRICE)
                    .setContractSignatureDate(correctContractSignatureDate)
                    .addBid(new MatchedBid()
                            .setIsWinning(true)
                            .addBidder(new MatchedBody()
                                    .setGroupId(BIDDER_GROUP_ID_1)))
                    .addBid(new MatchedBid()
                            .addBidder(new MatchedBody()
                                    .setGroupId(BIDDER_GROUP_ID_2)))
                    .addBid(new MatchedBid()
                            .addBidder(new MatchedBody()
                                    .setGroupId(BIDDER_GROUP_ID_3)))
                    .setBidsCount(3));
    private final MatchedTender tender2 = new MatchedTender()
            .addLot(new MatchedTenderLot()
                    .setStructuredId(new StructuredLotId()
                            .setTenderId(TENDER2_ID))
                    .setSelectionMethod(SelectionMethod.LOWEST_PRICE)
                    .setContractNumber("1")
                    .setContractSignatureDate(correctContractSignatureDate)
                    .addBid(new MatchedBid()
                            .addBidder(new MatchedBody()
                                    .setGroupId(BIDDER_GROUP_ID_3)))
                    .addBid(new MatchedBid()
                            .addBidder(new MatchedBody()
                                    .setGroupId(BIDDER_GROUP_ID_2)))
                    .addBid(new MatchedBid()
                            .setIsWinning(true)
                            .addBidder(new MatchedBody()
                                    .setGroupId(BIDDER_GROUP_ID_1)))
                    .setBidsCount(3));
    private final MatchedTender tender3 = new MatchedTender()
            .addLot(new MatchedTenderLot()
                    .setStructuredId(new StructuredLotId()
                            .setTenderId(TENDER3_ID))
                    .setSelectionMethod(SelectionMethod.LOWEST_PRICE)
                    .setContractNumber("1")
                    .setContractSignatureDate(correctContractSignatureDate)
                    .addBid(new MatchedBid()
                            .addBidder(new MatchedBody()
                                    .setGroupId(BIDDER_GROUP_ID_1)))
                    .setBidsCount(1))
            .addLot(new MatchedTenderLot()
                    .setStructuredId(new StructuredLotId()
                            .setTenderId(TENDER3_ID))
                    .setSelectionMethod(SelectionMethod.MEAT)
                    .setContractSignatureDate(incorrectContractSignatureDate)
                    .addBid(new MatchedBid()
                            .setIsWinning(true)
                            .addBidder(new MatchedBody()
                                    .setGroupId(BIDDER_GROUP_ID_4)))
                    .setBidsCount(1));

    /**
     * Default constructor to set tender IDs. The setter does not support fluent interface, so we call it here.
     */
    public TenderLotPluginTest() {
        tender1.setId(TENDER1_ID);
        tender2.setId(TENDER2_ID);
        tender3.setId(TENDER3_ID);
    }

    /**
     * Test of master bid count.
     */
    @Test
    public void masterLotCountTest() {
        final BaseTenderLotPlugin lotPlugin = new TenderLotPlugin();
        MasterTender masterTender = new MasterTender();
        lotPlugin.master(Arrays.asList(tender1, tender2, tender3),
                masterTender,
                Arrays.asList(tender1, tender2, tender3));

        // There are four lots:
        //  1-1  2-1  3-1
        //            3-2
        // Pairs and its matching scores:
        // (1-1,2-1) .. MS = 1+1+1+1, C = 4 -> MR = 4/4 = 1
        // (1-1,3-1) .. MS = 0+1+1,   C = 3 -> MR = 2/3 = 0.6666
        // (1-1,3-2) .. MS = 0+0+0+0, C = 4 -> MR = 0/4 = 0
        // (2-1,3-1) .. MS = 0+1+1+1, C = 4 -> MR = 3/4 = 0.75
        // (2-1,3-2) .. MS = 0+0+0+0, C = 4 -> MR = 0/4 = 0
        // Group creation:
        // 1.step: group (1-1,2-1)
        // 2.step: group (1-1,2-1,3-1)
        // 3.step: group (1-1,2-1,3-1) and group (3-2)
        // Two groups are created so master tender has two lots (one master lot from one group of matched tenders)
        assertTrue(masterTender.getLots().size() == 2);
    }

    /**
     * Test of master fundings.
     */
    @Test
    public void masterEmptyFundingsTest() {
        final FundingsPlugin fundingsPlugin = new FundingsPlugin<>();
        MasterTender masterTender = new MasterTender();
        fundingsPlugin.master(Arrays.asList(tender1, tender2, tender3),
                masterTender,
                Arrays.asList(tender1, tender2, tender3));

        assertNull(masterTender.getFundings());
    }
}
