package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dto.codetables.TenderLotStatus;
import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.indicator.IndicatorStatus;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * Test of corruption single bid indicator plugin.
 *
 * @author Tomas Mrazek
 */
public final class LotSingleBidIndicatorPluginTest {

    private final LotSingleBidIndicatorPlugin plugin = new LotSingleBidIndicatorPlugin();

    /**
     * Test of undefined indicators.
     */
    @Test
    public void undefinedIndicatorTest() {
        assertEquals(plugin.evaluate(new MasterTenderLot().setStatus(TenderLotStatus.CANCELLED)).getStatus(),
            IndicatorStatus.UNDEFINED);
    }
    
    /**
     * Test of insufficient indicators.
     */
    @Test
    public void insufficientIndicatorTest() {
        assertEquals(plugin.evaluate(new MasterTenderLot().setStatus(TenderLotStatus.AWARDED)).getStatus(),
            IndicatorStatus.INSUFFICIENT_DATA);
    }

    /**
     * Test of calculated indicators.
     */
    @Test
    public void calculatedIndicatorTest() {
        Indicator indicator = plugin.evaluate(new MasterTenderLot().setStatus(TenderLotStatus.AWARDED).setValidBidsCount(1));
        assertEquals(indicator.getStatus(), IndicatorStatus.CALCULATED);
        assertEquals(indicator.getValue(), Double.valueOf(0));
        assertEquals(indicator.getType(), TenderIndicatorType.INTEGRITY_SINGLE_BID.name());

        indicator = plugin.evaluate(new MasterTenderLot().setStatus(TenderLotStatus.AWARDED).setBidsCount(1));
        assertEquals(indicator.getStatus(), IndicatorStatus.CALCULATED);
        assertEquals(indicator.getValue(), Double.valueOf(0));
        assertEquals(indicator.getType(), TenderIndicatorType.INTEGRITY_SINGLE_BID.name());

        indicator = plugin.evaluate(new MasterTenderLot().setStatus(TenderLotStatus.AWARDED).setValidBidsCount(1).setBidsCount(10));
        assertEquals(indicator.getStatus(), IndicatorStatus.CALCULATED);
        assertEquals(indicator.getValue(), Double.valueOf(0));
        assertEquals(indicator.getType(), TenderIndicatorType.INTEGRITY_SINGLE_BID.name());

        indicator = plugin.evaluate(new MasterTenderLot().setStatus(TenderLotStatus.AWARDED).setValidBidsCount(10).setBidsCount(1));
        assertEquals(indicator.getStatus(), IndicatorStatus.CALCULATED);
        assertEquals(indicator.getValue(), Double.valueOf(100));
        assertEquals(indicator.getType(), TenderIndicatorType.INTEGRITY_SINGLE_BID.name());

        indicator = plugin.evaluate(new MasterTenderLot().setStatus(TenderLotStatus.AWARDED).setBidsCount(10));
        assertEquals(indicator.getStatus(), IndicatorStatus.CALCULATED);
        assertEquals(indicator.getValue(), Double.valueOf(100));
        assertEquals(indicator.getType(), TenderIndicatorType.INTEGRITY_SINGLE_BID.name());
    }
}
