package eu.dl.worker.master.utils.price;


import eu.dl.dataaccess.dto.generic.Price;
import eu.dl.dataaccess.dto.master.MasterBid;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import eu.dl.worker.master.plugin.specific.DigiwhistPricePlugin;
import org.junit.Test;

import java.math.BigDecimal;
import java.util.Arrays;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertNotNull;

/**
 * Tests of DigiwhistPricePlugin.
 *
 * @author Tomas Mrazek
 */
public final class DigiwhistPricePluginTest {
    private final DigiwhistPricePlugin plugin;

    /**
     * Plugin initialization.
     */
    public DigiwhistPricePluginTest() {
        plugin = new DigiwhistPricePlugin();
    }

    /**
     * Tests methods with null parameters/data.
     */
    @Test
    public void nullValuesTest() {
        assertNull(plugin.master(null, null, null));

        assertNotNull(plugin.master(null, new MasterTender(), null));

        assertNotNull(plugin.master(null, new MasterTender().setLots(Arrays.asList(
            new MasterTenderLot().setBids(Arrays.asList(new MasterBid().setIsWinning(true))),
            new MasterTenderLot()
        )), null));
    }

    /**
     * Test of selection the best price.
     */
    @Test
    public void firstNotNullTest() {
        //plugin should be selects first not empty amount from price attributes in following order: netAmountEur, maxNetAmount, minNetAmount
        MasterTender tender = new MasterTender();
        tender.setFinalPrice(new Price().setMinNetAmount(new BigDecimal(300)));

        // best price - minNetAmount
        tender = plugin.master(null, tender, null);
        assertEquals(new BigDecimal(300), tender.getDigiwhistPrice().getNetAmountEur());

        // best price - maxNetAmount
        tender.getFinalPrice().setMaxNetAmount(new BigDecimal(200));
        tender = plugin.master(null, tender, null);
        assertEquals(new BigDecimal(200), tender.getDigiwhistPrice().getNetAmountEur());

        // best price - netAmountEur
        tender.getFinalPrice().setNetAmountEur(new BigDecimal(100));
        tender = plugin.master(null, tender, null);
        assertEquals(new BigDecimal(100), tender.getDigiwhistPrice().getNetAmountEur());
    }

    /**
     * More complex test of plugin.
     */
    @Test
    public void tenderDigiwhistPriceTest() {
        MasterTender tender = new MasterTender();
        tender.setLots(Arrays.asList(
            new MasterTenderLot().setBids(Arrays.asList(
                new MasterBid().setIsWinning(true).setPrice(new Price().setNetAmountEur(new BigDecimal(100))),
                new MasterBid().setIsWinning(true).setPrice(new Price().setNetAmountEur(new BigDecimal(200))))),
            new MasterTenderLot().setBids(Arrays.asList(new MasterBid().setIsWinning(true).setPrice(new Price()
                .setNetAmountEur(new BigDecimal(200))))),
            new MasterTenderLot().setBids(Arrays.asList(new MasterBid().setIsWinning(false).setPrice(new Price()
                .setNetAmountEur(new BigDecimal(100)))))));

        tender = plugin.master(null, tender, null);
        assertEquals(new BigDecimal(500), tender.getDigiwhistPrice().getNetAmountEur());
        assertEquals(new BigDecimal(100), tender.getLots().get(0).getBids().get(0).getDigiwhistPrice().getNetAmountEur());
        assertEquals(new BigDecimal(200), tender.getLots().get(0).getBids().get(1).getDigiwhistPrice().getNetAmountEur());
        assertEquals(new BigDecimal(200), tender.getLots().get(1).getBids().get(0).getDigiwhistPrice().getNetAmountEur());


        tender = new MasterTender();
        tender.setLots(Arrays.asList(
            new MasterTenderLot().setEstimatedPrice(new Price().setNetAmountEur(new BigDecimal(100)))
                .setBids(Arrays.asList(new MasterBid().setIsWinning(true))),
            // price is equally split between winning bids
            new MasterTenderLot().setEstimatedPrice(new Price().setNetAmountEur(new BigDecimal(300)))
                .setBids(Arrays.asList(new MasterBid().setIsWinning(true), new MasterBid().setIsWinning(true)))
        ));

        tender = plugin.master(null, tender, null);
        assertEquals(100, tender.getLots().get(0).getBids().get(0).getDigiwhistPrice().getNetAmountEur().doubleValue(), 0);
        assertEquals(150, tender.getLots().get(1).getBids().get(0).getDigiwhistPrice().getNetAmountEur().doubleValue(), 0);
        assertEquals(150, tender.getLots().get(1).getBids().get(1).getDigiwhistPrice().getNetAmountEur().doubleValue(), 0);
    }
}
