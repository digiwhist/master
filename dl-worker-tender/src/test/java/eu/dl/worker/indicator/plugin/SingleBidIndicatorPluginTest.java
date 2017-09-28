package eu.dl.worker.indicator.plugin;

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
public final class SingleBidIndicatorPluginTest {

    private final MasterTender nullTender = new MasterTender()
            .setLots(Arrays.asList(new MasterTenderLot()));

    private final MasterTender tender1 = new MasterTender()
            .setLots(Arrays.asList(new MasterTenderLot().setValidBidsCount(0)));

    private final MasterTender tender2 = new MasterTender()
            .setLots(Arrays.asList(new MasterTenderLot()
                    .setValidBidsCount(2)));

    private final MasterTender tender3 = new MasterTender()
            .setLots(Arrays.asList(new MasterTenderLot()
                    .setValidBidsCount(1)));

    private final MasterTender tender4 = new MasterTender()
            .setLots(Arrays.asList(new MasterTenderLot()
                    .setBidsCount(0)));

    private final MasterTender tender5 = new MasterTender()
            .setLots(Arrays.asList(new MasterTenderLot()
                    .setBidsCount(1)));

    private final SingleBidIndicatorPlugin plugin = new SingleBidIndicatorPlugin();

    /**
     * Test of correct tender address.
     */
    @Test
    public void noIndicatorTest() {
        assertNull(plugin.evaulate(null));
        assertNull(plugin.evaulate(nullTender));
        assertNull(plugin.evaulate(tender1));
        assertNull(plugin.evaulate(tender2));
        assertNull(plugin.evaulate(tender4));
    }

    /**
     * Test of positive result.
     */
    @Test
    public void okTest() {
        assertEquals(plugin.evaulate(tender3).getType(),
                TenderIndicatorType.CORRUPTION_SINGLE_BID.name());
        assertEquals(plugin.evaulate(tender5).getType(),
                TenderIndicatorType.CORRUPTION_SINGLE_BID.name());
    }

    /**
     * Test of correct plugin type.
     */
    @Test
    public void getTypeTest() {
        assertEquals(plugin.getType(),
                TenderIndicatorType.CORRUPTION_SINGLE_BID.name());
    }
}
