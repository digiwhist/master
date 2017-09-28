package eu.dl.worker.indicator.plugin;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.util.Arrays;

import org.junit.Before;
import org.junit.Test;

import eu.dl.core.config.Config;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.MasterTender;

/**
 * Test of electronic auction indicator plugin.
 *
 * @author Jakub Krafka
 */
public final class CallForTenderIndicatorPluginTest {

    private final MasterTender nullTender = new MasterTender();

    private final MasterTender tender1 = new MasterTender()
            .setCountry("CC")
            .setPublications(null);

    private final MasterTender tender4 = new MasterTender()
            .setCountry("CZ")
            .setPublications(Arrays.asList(
                    new Publication().setFormType(PublicationFormType.CONTRACT_AWARD),
                    new Publication().setFormType(PublicationFormType.CONTRACT_NOTICE)));

    private final MasterTender tender2 = new MasterTender()
            .setCountry("CZ")
            .setPublications(null);

    private final MasterTender tender3 = new MasterTender()
            .setCountry("CZ")
            .setPublications(Arrays.asList(
                    new Publication().setFormType(PublicationFormType.CONTRACT_AWARD),
                    new Publication().setFormType(PublicationFormType.CONTRACT_IMPLEMENTATION)));


    private final CallForTenderIndicatorPlugin plugin = new CallForTenderIndicatorPlugin();

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
        assertNull(plugin.evaulate(tender1));
    }

    /**
     * Test of positive result.
     */
    @Test
    public void okTest() {
        assertEquals(plugin.evaulate(tender2).getType(),
                TenderIndicatorType.CORRUPTION_PRIOR_INFORMATION_NOTICE.name());
        assertEquals(plugin.evaulate(tender3).getType(),
                TenderIndicatorType.CORRUPTION_PRIOR_INFORMATION_NOTICE.name());
    }

    /**
     * Test of correct plugin type.
     */
    @Test
    public void getTypeTest() {
        assertEquals(plugin.getType(),
                TenderIndicatorType.CORRUPTION_PRIOR_INFORMATION_NOTICE.name());
    }
}
