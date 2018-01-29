package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dto.indicator.IndicatorStatus;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.MasterTender;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

/**
 * Test of centralized procurement.
 *
 * @author Jakub Krafka
 */
public final class CentralizedProcurementIndicatorPluginTest {

    private final MasterTender nullTender = new MasterTender();

    private final MasterTender tender1 = new MasterTender()
                    .setIsCentralProcurement(true);

    private final MasterTender tender2 = new MasterTender()
            .setIsCentralProcurement(false);


    private final CentralizedProcurementIndicatorPlugin plugin
            = new CentralizedProcurementIndicatorPlugin();

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
    public void isCentralProcurementTest() {
        assertEquals(plugin.evaluate(tender1).getType(),
                TenderIndicatorType.ADMINISTRATIVE_CENTRALIZED_PROCUREMENT.name());
        assertEquals(plugin.evaluate(tender1).getValue(), Double.valueOf(100));
        assertEquals(plugin.evaluate(tender1).getStatus(), IndicatorStatus.CALCULATED);
    }

    /**
     * Test of positive result.
     */
    @Test
    public void isNotCentralProcurementTest() {
        assertEquals(plugin.evaluate(tender2).getType(),
                TenderIndicatorType.ADMINISTRATIVE_CENTRALIZED_PROCUREMENT.name());
        assertEquals(plugin.evaluate(tender2).getValue(), Double.valueOf(0));
        assertEquals(plugin.evaluate(tender2).getStatus(), IndicatorStatus.CALCULATED);
    }

    /**
     * Test of positive result.
     */
    @Test
    public void nullCentralProcurementTest() {
        assertEquals(plugin.evaluate(tender2).getType(),
                TenderIndicatorType.ADMINISTRATIVE_CENTRALIZED_PROCUREMENT.name());
        assertNull(plugin.evaluate(nullTender).getValue());
        assertEquals(plugin.evaluate(nullTender).getStatus(), IndicatorStatus.INSUFFICIENT_DATA);
    }

    /**
     * Test of correct plugin type.
     */
    @Test
    public void getTypeTest() {
        assertEquals(plugin.getType(),
                TenderIndicatorType.ADMINISTRATIVE_CENTRALIZED_PROCUREMENT.name());
    }
}
