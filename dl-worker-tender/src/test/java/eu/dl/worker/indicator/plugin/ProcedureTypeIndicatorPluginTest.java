package eu.dl.worker.indicator.plugin;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.util.Arrays;

import org.junit.Before;
import org.junit.Test;

import eu.dl.core.config.Config;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.MasterTender;

/**
 * Test of electronic auction indicator plugin.
 *
 * @author Jakub Krafka
 */
public final class ProcedureTypeIndicatorPluginTest {

    private final MasterTender nullTender = new MasterTender();

    private final MasterTender tender1 = new MasterTender()
                    .setCountry("CZ");
    
    private final MasterTender tender2 = new MasterTender()
            .setCountry("SK");

    private final MasterTender tender3 = new MasterTender()
            .setCountry("AT")
            .setProcedureType(TenderProcedureType.RESTRICTED);
    
    private final MasterTender tender4 = new MasterTender()
            .setCountry("AT")
            .setProcedureType(TenderProcedureType.NEGOTIATED_WITH_PUBLICATION);

    private final ProcedureTypeIndicatorPlugin plugin = new ProcedureTypeIndicatorPlugin();

    /**
     * Test initialization.
     */
    @Before
    public void init() {
        Config.getInstance().setConfigFile(Arrays.asList("unit_test"));
    }
    
    /**
     * Test of correct tender address.
     */
    @Test
    public void noIndicatorTest() {
        assertNull(plugin.evaulate(null));
        assertNull(plugin.evaulate(nullTender));
        assertNull(plugin.evaulate(tender1));
        assertNull(plugin.evaulate(tender2));
        assertNull(plugin.evaulate(tender3));
    }

    /**
     * Test of positive result.
     */
    @Test
    public void okTest() {
        assertEquals(plugin.evaulate(tender4).getType(),
                TenderIndicatorType.CORRUPTION_PROCEDURE_TYPE.name());
    }

    /**
     * Test of correct plugin type.
     */
    @Test
    public void getTypeTest() {
        assertEquals(plugin.getType(),
                TenderIndicatorType.CORRUPTION_PROCEDURE_TYPE.name());
    }
}
