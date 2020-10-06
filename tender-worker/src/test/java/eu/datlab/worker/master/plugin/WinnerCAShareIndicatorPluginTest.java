package eu.datlab.worker.master.plugin;

import eu.dl.dataaccess.dto.generic.Price;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.indicator.IndicatorStatus;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.MasterBid;
import eu.dl.dataaccess.dto.master.MasterBody;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import org.junit.Test;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

/**
 * Test for WinnerCAShareIndicatorPlugin.
 */
public class WinnerCAShareIndicatorPluginTest {

    private final List<Publication> publications =
            new ArrayList<Publication>() {{
                add(new Publication().setIsIncluded(true).setPublicationDate(LocalDate.of(2019, 4, 4)));
                add(new Publication().setIsIncluded(false).setPublicationDate(LocalDate.of(2019, 3, 3)));
                add(new Publication().setIsIncluded(true).setPublicationDate(LocalDate.of(2019, 2, 2)));
            }};

    // no buyer
    private final MasterTender tender1 = new MasterTender().addLot(new MasterTenderLot()
            .addBid(new MasterBid().setIsWinning(true).setBidders(Collections.singletonList(new MasterBody().setGroupId("s1")))))
            .setPublications(publications);

    // no lot
    private final MasterTender tender2 = new MasterTender().addBuyer(new MasterBody().setGroupId("B1")).setPublications(publications);

    // buyer1, different lots:
    //                      lot with two bids,
    //                      lot without bidder,
    //                      two lots with winning bid (supplier1 and supplier2) and
    //                      lot without winning bid
    private final MasterTender tender3 = new MasterTender()
            .addBuyer(new MasterBody().setGroupId("B1"))
            // more than one bid
            .addLot(new MasterTenderLot()
                    .addBid(new MasterBid())
                    .addBid(new MasterBid()))
            // no bidder
            .addLot(new MasterTenderLot()
                    .addBid(new MasterBid()
                            .setIsWinning(true)
                            .setPrice(new Price().setNetAmountNational(new BigDecimal(1000)))))
            // ok, buyer1, supplier1
            .addLot(new MasterTenderLot()
                    .addBid(new MasterBid()
                            .setBidders(Collections.singletonList(new MasterBody()
                                    .setGroupId("S1")))
                            .setIsWinning(true)
                            .setPrice(new Price().setNetAmountNational(new BigDecimal(20000)))))
            // ok, buyer1, supplier2
            .addLot(new MasterTenderLot()
                    .addBid(new MasterBid()
                            .setBidders(Collections.singletonList(new MasterBody()
                                    .setGroupId("S2")))
                            .setIsWinning(true)
                            .setPrice(new Price().setNetAmountNational(new BigDecimal(30000)))))
            // no winning bid
            .addLot(new MasterTenderLot()
                    .addBid(new MasterBid()
                            .setBidders(Collections.singletonList(new MasterBody()
                                    .setGroupId("S3")))
                            .setIsWinning(false)
                            .setPrice(new Price().setNetAmountNational(new BigDecimal(10000)))))
            .setPublications(publications);

    // buyer1, supplier3
    private final MasterTender tender4 = new MasterTender()
            .addBuyer(new MasterBody().setGroupId("B1"))
            .addLot(new MasterTenderLot()
                    .addBid(new MasterBid()
                            .setBidders(Collections.singletonList(new MasterBody()
                                    .setGroupId("S3")))
                            .setIsWinning(true)
                            .setPrice(new Price().setNetAmountNational(new BigDecimal(50000)))))
            .setPublications(publications);

    // buyer2, supplier1
    private final MasterTender tender5 = new MasterTender()
            .addBuyer(new MasterBody().setGroupId("B2"))
            .addLot(new MasterTenderLot()
                    .addBid(new MasterBid()
                            .setBidders(Collections.singletonList(new MasterBody()
                                    .setGroupId("S1")))
                            .setIsWinning(true)
                            .setPrice(new Price().setNetAmountNational(new BigDecimal(10000)))))
            .setPublications(publications);

    // more than one buyer
    private final MasterTender tender6 = new MasterTender()
            .addBuyer(new MasterBody().setGroupId("B1"))
            .addBuyer(new MasterBody().setGroupId("B2"))
            .addLot(new MasterTenderLot()
                    .addBid(new MasterBid()
                            .setBidders(Collections.singletonList(new MasterBody()
                                    .setGroupId("S1")))
                            .setIsWinning(true)
                            .setPrice(new Price().setNetAmountNational(new BigDecimal(10000)))))
            .setPublications(publications);

    // no publications
    private final MasterTender tender7 = new MasterTender().addBuyer(new MasterBody().setGroupId("B1")).addLot(new MasterTenderLot()
            .addBid(new MasterBid().setIsWinning(true).setBidders(Collections.singletonList(new MasterBody().setGroupId("s1")))));

    private final List<MasterTender> buyer1 =
            new ArrayList<MasterTender>() {{
                add(tender2);
                add(tender3);
                add(tender4);
            }};
    private final List<MasterTender> buyer2 = Collections.singletonList(tender5);
    private final List<MasterTender> supplier1buyer1 = Collections.singletonList(tender3);
    private final List<MasterTender> supplier1buyer2 = Collections.singletonList(tender5);
    private final List<MasterTender> supplier2buyer1 = Collections.singletonList(tender3);
    private final List<MasterTender> supplier3buyer1 = Collections.singletonList(tender4);

    private final  WinnerCAShareIndicatorPlugin winnerSharePlugin;

    /**
     * Constructor.
     */
    public WinnerCAShareIndicatorPluginTest() {
        winnerSharePlugin = new WinnerCAShareIndicatorPlugin();
    }

    /**
     * Init test.
     */
    @Test
    public final void initTest() {
        assertNotNull(winnerSharePlugin.getType());
        assertEquals(winnerSharePlugin.getType(), TenderIndicatorType.INTEGRITY_WINNER_CA_SHARE.name());

        assertNotNull(winnerSharePlugin.getCacheTotalValuesForBuyersPerYears());
        assertNotNull(winnerSharePlugin.getCacheTotalValuesForSuppliersByBuyers());

        assertTrue(winnerSharePlugin.getCacheTotalValuesForBuyersPerYears().isEmpty());
        assertTrue(winnerSharePlugin.getCacheTotalValuesForSuppliersByBuyers().isEmpty());
    }

    /**
     * Test of insufficient inputs.
     */
    @Test
    public final void insufficientTest() {
        // no buyer
        assertEquals(IndicatorStatus.INSUFFICIENT_DATA,
                winnerSharePlugin.evaluate(tender1.getLots().get(0), tender1).getStatus());

        // more than one bid
        assertEquals(IndicatorStatus.INSUFFICIENT_DATA,
                winnerSharePlugin.evaluate(tender3.getLots().get(0), tender3).getStatus());

        // no supplier
        assertEquals(IndicatorStatus.INSUFFICIENT_DATA,
                winnerSharePlugin.evaluate(tender3.getLots().get(1), tender3).getStatus());

        // no winning bid
        assertEquals(IndicatorStatus.INSUFFICIENT_DATA,
                winnerSharePlugin.evaluate(tender3.getLots().get(4), tender3).getStatus());

        // more than one buyer
        assertEquals(IndicatorStatus.INSUFFICIENT_DATA,
                winnerSharePlugin.evaluate(tender6.getLots().get(0), tender6).getStatus());

        // no publications
        assertEquals(IndicatorStatus.INSUFFICIENT_DATA,
                winnerSharePlugin.evaluate(tender7.getLots().get(0), tender7).getStatus());
    }


    /**
     * Test of hasGivenSupplierAndPrice.
     */
    @Test
    public final void hasGivenSupplierAndPriceTest() {
        // ok
        assertTrue(winnerSharePlugin.hasGivenSupplierAndPrice(new MasterBid()
                .setPrice(new Price().setNetAmountNational(new BigDecimal(100)))
                .setBidders(Collections.singletonList(new MasterBody().setGroupId("S1"))), "S1"));

        // more than one supplier
        assertFalse(winnerSharePlugin.hasGivenSupplierAndPrice(new MasterBid()
                .setPrice(new Price().setNetAmountNational(new BigDecimal(100)))
                .setBidders(Arrays.asList(new MasterBody().setGroupId("S1"), new MasterBody().setGroupId("S2"))), "S1"));

        // no price available
        assertFalse(winnerSharePlugin.hasGivenSupplierAndPrice(new MasterBid()
                .setBidders(Collections.singletonList(new MasterBody().setGroupId("S1"))), "S1"));

        // no bidder
        assertFalse(winnerSharePlugin.hasGivenSupplierAndPrice(new MasterBid()
                .setPrice(new Price().setNetAmountNational(new BigDecimal(100))), "S1"));

        // no bidder
        assertFalse(winnerSharePlugin.hasGivenSupplierAndPrice(new MasterBid()
                .setPrice(new Price().setNetAmountNational(new BigDecimal(100)))
                .setBidders(new ArrayList<>()), "S1"));

        // bidder with another group id
        assertFalse(winnerSharePlugin.hasGivenSupplierAndPrice(new MasterBid()
                .setPrice(new Price().setNetAmountNational(new BigDecimal(100)))
                .setBidders(Collections.singletonList(new MasterBody().setGroupId("S2"))), "S1"));
    }


    /**
     * Test of correct inputs, calculated.
     */
    @Test
    public final void calculatedTest() {
        assertEquals(50.0, winnerSharePlugin.computeIndicatorValue(1000D, 2000D), 0.001);
        assertEquals(0, winnerSharePlugin.computeIndicatorValue(1000D, 1000D), 0.001);
        assertEquals(100, winnerSharePlugin.computeIndicatorValue(0D, 2000D), 0.001);
    }

    /**
     * Test of computeTotalValueForSupplierFromMasterTenders.
     */
    @Test
    public final void computeTotalValueForSupplierFromMasterTendersTest() {
        assertEquals(20000.0, winnerSharePlugin.computeTotalValueForSupplierFromMasterTenders(supplier1buyer1, "S1"), 0.001);
        assertEquals(10000.0, winnerSharePlugin.computeTotalValueForSupplierFromMasterTenders(supplier1buyer2, "S1"),
                0.001);
        assertEquals(30000.0, winnerSharePlugin.computeTotalValueForSupplierFromMasterTenders(supplier2buyer1, "S2"),
                0.001);
        assertEquals(50000, winnerSharePlugin.computeTotalValueForSupplierFromMasterTenders(supplier3buyer1, "S3"), 0.001);
        assertEquals(0, winnerSharePlugin.computeTotalValueForSupplierFromMasterTenders(supplier3buyer1,
                "anotherGroupId"), 0.001);
    }

    /**
     * Test of computeTotalValueForSupplierFromMasterTenders.
     */
    @Test
    public final void computeTotalValueForBuyerFromMasterTendersTest() {
        assertEquals(101000.0, winnerSharePlugin.computeTotalValueForBuyerFromMasterTenders(buyer1), 0.001);
        assertEquals(10000.0, winnerSharePlugin.computeTotalValueForBuyerFromMasterTenders(buyer2), 0.001);
    }

    /**
     * Test of cache updates.
     */
    @Test
    public final void updatesCacheTest() {
        // Supplier
        // empty in the beginning
        assertTrue(winnerSharePlugin.getCacheTotalValuesForSuppliersByBuyers().isEmpty());

        // update
        winnerSharePlugin.getCacheTotalValuesForSuppliersByBuyers().update("S1", "B1", 2018, 10000D);
        assertFalse(winnerSharePlugin.getCacheTotalValuesForSuppliersByBuyers().isEmpty());
        // get updated result
        Double result1 = winnerSharePlugin.getCacheTotalValuesForSuppliersByBuyers().getValueForSupplierPerBuyerAndYear("S1",
                "B1", 2018);
        assertNotNull(result1);
        assertEquals(10000, result1, 0.001);

        winnerSharePlugin.getCacheTotalValuesForSuppliersByBuyers().update("S1", "B2", 2018, 20000D);
        Double result2 =
                winnerSharePlugin.getCacheTotalValuesForSuppliersByBuyers().getValueForSupplierPerBuyerAndYear("S1",
                        "B2", 2018);
        assertNotNull(result2);
        assertEquals(20000, result2, 0.001);

        // update existing, not used in plugin
        winnerSharePlugin.getCacheTotalValuesForSuppliersByBuyers().update("S1", "B2", 2018, 30000D);
        Double result3 =
                winnerSharePlugin.getCacheTotalValuesForSuppliersByBuyers().getValueForSupplierPerBuyerAndYear("S1",
                        "B2", 2018);
        assertNotNull(result3);
        assertEquals(30000, result3, 0.001);

        // Buyer
        assertTrue(winnerSharePlugin.getCacheTotalValuesForBuyersPerYears().isEmpty());

        winnerSharePlugin.getCacheTotalValuesForBuyersPerYears().update("B1", 2018, 10000D);
        assertFalse(winnerSharePlugin.getCacheTotalValuesForBuyersPerYears().isEmpty());
        Double result4 =
                winnerSharePlugin.getCacheTotalValuesForBuyersPerYears().getValueForBuyerPerYear("B1", 2018);
        assertNotNull(result4);
        assertEquals(10000, result4, 0.001);

        winnerSharePlugin.getCacheTotalValuesForBuyersPerYears().update("B1", 2019, 20000D);
        assertFalse(winnerSharePlugin.getCacheTotalValuesForBuyersPerYears().isEmpty());
        Double result5 =
                winnerSharePlugin.getCacheTotalValuesForBuyersPerYears().getValueForBuyerPerYear("B1", 2019);
        assertNotNull(result5);
        assertEquals(20000, result5, 0.001);

        // not existing data
        Double result6 = winnerSharePlugin.getCacheTotalValuesForBuyersPerYears().getValueForBuyerPerYear("B1", 2020);
        Double result7 = winnerSharePlugin.getCacheTotalValuesForBuyersPerYears().getValueForBuyerPerYear("B2", 2019);
        assertNull(result6);
        assertNull(result7);
    }

}
