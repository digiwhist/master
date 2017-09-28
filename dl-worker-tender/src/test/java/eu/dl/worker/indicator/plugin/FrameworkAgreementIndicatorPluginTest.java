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
public final class FrameworkAgreementIndicatorPluginTest {

    private final MasterTender nullTender = new MasterTender()
            .setLots(Arrays.asList(new MasterTenderLot()));

    private final MasterTender tender1 = new MasterTender()
                    .setIsFrameworkAgreement(true);

    private final MasterTender tender2 = new MasterTender()
            .setLots(Arrays.asList(new MasterTenderLot()
                    .setIsFrameworkAgreement(true)));

    private final MasterTender tender3 = new MasterTender()
            .setLots(Arrays.asList(new MasterTenderLot()
                    .setIsFrameworkAgreement(false)));

    private final FrameworkAgreementIndicatorPlugin plugin = new FrameworkAgreementIndicatorPlugin();

    /**
     * Test of correct tender address.
     */
    @Test
    public void noIndicatorTest() {
        assertNull(plugin.evaulate(null));
        assertNull(plugin.evaulate(nullTender));
        assertNull(plugin.evaulate(tender3));
    }

    /**
     * Test of positive result.
     */
    @Test
    public void okTest() {
        assertEquals(plugin.evaulate(tender1).getType(),
                TenderIndicatorType.ADMINISTRATIVE_FRAMEWORK_AGREEMENT.name());
        assertEquals(plugin.evaulate(tender2).getType(),
                TenderIndicatorType.ADMINISTRATIVE_FRAMEWORK_AGREEMENT.name());
    }

    /**
     * Test of correct plugin type.
     */
    @Test
    public void getTypeTest() {
        assertEquals(plugin.getType(),
                TenderIndicatorType.ADMINISTRATIVE_FRAMEWORK_AGREEMENT.name());
    }
}
