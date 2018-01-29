package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dto.indicator.IndicatorStatus;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import org.junit.Test;

import java.util.Arrays;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

/**
 * Test of electronic auction indicator plugin.
 *
 * @author Jakub Krafka
 */
public final class ElectronicAuctionIndicatorPluginTest {

    private final MasterTender nullTender = new MasterTender()
            .setLots(Arrays.asList(new MasterTenderLot()));

    private final MasterTender tender1 = new MasterTender()
                    .setIsElectronicAuction(true);

    private final MasterTender tender2 = new MasterTender()
            .setIsElectronicAuction(false);

    private final MasterTender tender3 = new MasterTender()
            .setIsElectronicAuction(false)
            .setLots(Arrays.asList(new MasterTenderLot()
                    .setIsElectronicAuction(true)));

    private final MasterTender tender4 = new MasterTender()
            .setIsElectronicAuction(true)
            .setLots(Arrays.asList(new MasterTenderLot()
                    .setIsElectronicAuction(false)));

    private final ElectronicAuctionIndicatorPlugin plugin = new ElectronicAuctionIndicatorPlugin();

    /**
     * Test of correct tender address.
     */
    @Test
    public void noIndicatorTest() {
        assertNull(plugin.evaluate(null));
    }

    /**
     * Test of positive result.
     */
    @Test
    public void isElectronicAuctionTest() {
        assertEquals(plugin.evaluate(tender1).getValue(), Double.valueOf(100));
        assertEquals(plugin.evaluate(tender1).getStatus(), IndicatorStatus.CALCULATED);
        assertEquals(plugin.evaluate(tender3).getValue(), Double.valueOf(100));
        assertEquals(plugin.evaluate(tender3).getStatus(), IndicatorStatus.CALCULATED);
        assertEquals(plugin.evaluate(tender4).getValue(), Double.valueOf(100));
        assertEquals(plugin.evaluate(tender4).getStatus(), IndicatorStatus.CALCULATED);
    }

    /**
     * Test of positive result.
     */
    @Test
    public void isNotElectronicAuctionTest() {
        assertEquals(plugin.evaluate(tender2).getValue(), Double.valueOf(0));
        assertEquals(plugin.evaluate(tender2).getStatus(), IndicatorStatus.CALCULATED);
    }

    /**
     * Test of positive result.
     */
    @Test
    public void nullElectronicAuctionTest() {
        assertNull(plugin.evaluate(nullTender).getValue());
        assertEquals(plugin.evaluate(nullTender).getStatus(), IndicatorStatus.INSUFFICIENT_DATA);
    }

    /**
     * Test of correct plugin type.
     */
    @Test
    public void typeTest() {
        assertEquals(plugin.getType(),
                TenderIndicatorType.ADMINISTRATIVE_ELECTRONIC_AUCTION.name());
        assertEquals(plugin.evaluate(tender1).getType(),
                TenderIndicatorType.ADMINISTRATIVE_ELECTRONIC_AUCTION.name());
        assertEquals(plugin.evaluate(tender2).getType(),
                TenderIndicatorType.ADMINISTRATIVE_ELECTRONIC_AUCTION.name());
    }
}
