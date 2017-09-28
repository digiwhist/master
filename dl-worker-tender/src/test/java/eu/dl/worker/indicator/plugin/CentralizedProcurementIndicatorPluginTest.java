package eu.dl.worker.indicator.plugin;

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
        assertNull(plugin.evaulate(null));
        assertNull(plugin.evaulate(nullTender));
        assertNull(plugin.evaulate(tender2));
    }

    /**
     * Test of positive result.
     */
    @Test
    public void okTest() {
        assertEquals(plugin.evaulate(tender1).getType(),
                TenderIndicatorType.ADMINISTRATIVE_CENTRALIZED_PROCUREMENT.name());
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
