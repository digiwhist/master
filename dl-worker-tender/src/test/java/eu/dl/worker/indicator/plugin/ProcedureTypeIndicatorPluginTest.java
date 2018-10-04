package eu.dl.worker.indicator.plugin;

import eu.dl.core.config.Config;
import eu.dl.dataaccess.dto.codetables.TenderProcedureType;
import eu.dl.dataaccess.dto.indicator.IndicatorStatus;
import eu.dl.dataaccess.dto.master.MasterTender;
import org.junit.Before;
import org.junit.Test;


import static org.junit.Assert.assertEquals;

/**
 * Test of procedure type indicator plugin.
 *
 * @author Jakub Krafka
 */
public final class ProcedureTypeIndicatorPluginTest {

    private final MasterTender nullTender = new MasterTender();

    private final MasterTender tender1 = new MasterTender()
            .setCountry("EE");
    
    private final MasterTender tender2 = new MasterTender()
            .setCountry("AT");

    private final MasterTender tender3 = new MasterTender()
            .setCountry("AT")
            .setProcedureType(TenderProcedureType.RESTRICTED);
    
    private final MasterTender tender4 = new MasterTender()
            .setCountry("AT")
            .setProcedureType(TenderProcedureType.NEGOTIATED_WITH_PUBLICATION);

    private final MasterTender tender5 = new MasterTender()
            .setCountry("AT")
            .setProcedureType(TenderProcedureType.NEGOTIATED_WITHOUT_PUBLICATION);

    private final ProcedureTypeIndicatorPlugin plugin = new ProcedureTypeIndicatorPlugin();

    /**
     * Test initialization.
     */
    @Before
    public void init() {
        Config.getInstance().addConfigFile("unit_test");
    }
    
    /**
     * Test of insufficient indicators.
     */
    @Test
    public void insufficientIndicatorTest() {
        assertEquals(plugin.evaluate(null).getStatus(), IndicatorStatus.INSUFFICIENT_DATA);
        assertEquals(plugin.evaluate(nullTender).getStatus(), IndicatorStatus.INSUFFICIENT_DATA);
    }

    /**
     * Test of calculated indicators.
     */
    @Test
    public void calculatedIndicatorTest() {
        assertEquals(plugin.evaluate(tender1).getStatus(), IndicatorStatus.CALCULATED);
        assertEquals(plugin.evaluate(tender1).getValue(), new Double(0d));

        assertEquals(plugin.evaluate(tender2).getStatus(), IndicatorStatus.CALCULATED);
        assertEquals(plugin.evaluate(tender2).getValue(), new Double(100d));

        assertEquals(plugin.evaluate(tender3).getStatus(), IndicatorStatus.CALCULATED);
        assertEquals(plugin.evaluate(tender3).getValue(), new Double(100d));

        assertEquals(plugin.evaluate(tender4).getStatus(), IndicatorStatus.CALCULATED);
        assertEquals(plugin.evaluate(tender4).getValue(), new Double(0d));

        assertEquals(plugin.evaluate(tender5).getStatus(), IndicatorStatus.CALCULATED);
        assertEquals(plugin.evaluate(tender5).getValue(), new Double(0d));
    }

}
