package eu.dl.worker.indicator.plugin;

import eu.dl.core.config.Config;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.indicator.IndicatorStatus;
import eu.dl.dataaccess.dto.master.MasterTender;
import org.junit.Before;
import org.junit.Test;

import java.util.Arrays;

import static org.junit.Assert.assertEquals;

/**
 * Test of call for tenders publication indicator plugin.
 *
 * @author Jakub Krafka
 */
public final class CallForTenderIndicatorPluginTest {

    private final MasterTender nullTender = new MasterTender();

    private final MasterTender tender1 = new MasterTender()
            .setPublications(Arrays.asList(
                    new Publication().setFormType(PublicationFormType.CONTRACT_AWARD),
                    new Publication().setFormType(PublicationFormType.CONTRACT_IMPLEMENTATION)));

    private final MasterTender tender2 = new MasterTender()
            .setCountry("CC")
            .setPublications(null);

    private final MasterTender tender3 = new MasterTender()
            .setCountry("CZ")
            .setPublications(null);

    private final MasterTender tender4 = new MasterTender()
            .setCountry("CZ")
            .setPublications(Arrays.asList(
                    new Publication().setFormType(PublicationFormType.CONTRACT_AWARD),
                    new Publication().setFormType(PublicationFormType.CONTRACT_IMPLEMENTATION)));

    private final MasterTender tender5 = new MasterTender()
            .setCountry("CZ")
            .setPublications(Arrays.asList(
                    new Publication().setFormType(PublicationFormType.CONTRACT_AWARD),
                    new Publication().setFormType(PublicationFormType.CONTRACT_NOTICE)));

    private final MasterTender tender6 = new MasterTender()
            .setCountry("BG")
            .setPublications(null);

    private final MasterTender tender7 = new MasterTender()
            .setPublications(Arrays.asList(
                    new Publication().setFormType(PublicationFormType.CONTRACT_AWARD),
                    new Publication().setFormType(PublicationFormType.CONTRACT_NOTICE)));

    private final CallForTenderIndicatorPlugin plugin = new CallForTenderIndicatorPlugin();

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
        assertEquals(plugin.evaluate(tender1).getStatus(), IndicatorStatus.INSUFFICIENT_DATA);
    }

    /**
     * Test of calculated indicators.
     */
    @Test
    public void calculatedIndicatorTest() {
        assertEquals(plugin.evaluate(tender2).getStatus(), IndicatorStatus.CALCULATED);
        assertEquals(plugin.evaluate(tender2).getValue(), new Double(100d));

        assertEquals(plugin.evaluate(tender3).getStatus(), IndicatorStatus.CALCULATED);
        assertEquals(plugin.evaluate(tender3).getValue(), new Double(0d));

        assertEquals(plugin.evaluate(tender4).getStatus(), IndicatorStatus.CALCULATED);
        assertEquals(plugin.evaluate(tender4).getValue(), new Double(0d));

        assertEquals(plugin.evaluate(tender5).getStatus(), IndicatorStatus.CALCULATED);
        assertEquals(plugin.evaluate(tender5).getValue(), new Double(100d));

        assertEquals(plugin.evaluate(tender6).getStatus(), IndicatorStatus.CALCULATED);
        assertEquals(plugin.evaluate(tender6).getValue(), new Double(100d));

        assertEquals(plugin.evaluate(tender7).getStatus(), IndicatorStatus.CALCULATED);
        assertEquals(plugin.evaluate(tender7).getValue(), new Double(100d));
    }

}
