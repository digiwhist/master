package eu.dl.worker.indicator.plugin;

import static eu.dl.core.ThrowableAssertion.assertThrown;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Arrays;

import org.junit.Before;
import org.junit.Test;

import eu.dl.core.config.Config;
import eu.dl.core.config.MisconfigurationException;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.MasterTender;

/**
 * Test of electronic auction indicator plugin.
 *
 * @author Jakub Krafka
 */
public final class AdvertisementPeriodIndicatorPluginTest {

    private final MasterTender nullTender = new MasterTender();

    private final MasterTender tender1 = new MasterTender()
            .setPublications(Arrays.asList(
                    new Publication().setFormType(PublicationFormType.CONTRACT_NOTICE),
                    new Publication().setFormType(PublicationFormType.CONTRACT_AMENDMENT)
                            .setPublicationDate(LocalDate.MAX)));

    private final MasterTender tender2 = new MasterTender()
            .setCountry("CZ")
            .setPublications(Arrays.asList(
                    new Publication().setFormType(PublicationFormType.CONTRACT_NOTICE),
                    new Publication().setFormType(PublicationFormType.CONTRACT_NOTICE)
                            .setPublicationDate(LocalDate.MAX)));

    private final MasterTender tender3 = new MasterTender()
            .setCountry("SK")
            .setPublications(Arrays.asList(
                    new Publication().setFormType(PublicationFormType.CONTRACT_NOTICE),
                    new Publication().setFormType(PublicationFormType.CONTRACT_NOTICE)
                            .setPublicationDate(LocalDate.MAX)));

    private final MasterTender tender4 = new MasterTender()
            .setCountry("CZ")
            .setPublications(Arrays.asList(
                    new Publication().setFormType(PublicationFormType.CONTRACT_NOTICE),
                    new Publication().setFormType(PublicationFormType.CONTRACT_NOTICE)
                            .setPublicationDate(LocalDate.MAX)));

    private final MasterTender tender5 = new MasterTender()
            .setCountry("CZ")
            .setBidDeadline(LocalDateTime.now())
            .setPublications(Arrays.asList(
                    new Publication().setFormType(PublicationFormType.CONTRACT_NOTICE)
                            .setPublicationDate(LocalDate.MAX),
                    new Publication().setFormType(PublicationFormType.CONTRACT_NOTICE)
                            .setPublicationDate(LocalDate.now())));

    private final MasterTender tender6 = new MasterTender()
            .setCountry("CZ")
            .setBidDeadline(LocalDateTime.now().minusDays(100))
            .setPublications(Arrays.asList(
                    new Publication().setFormType(PublicationFormType.CONTRACT_NOTICE),
                    new Publication().setFormType(PublicationFormType.CONTRACT_NOTICE)
                            .setPublicationDate(LocalDate.now())));

    private final MasterTender tender7 = new MasterTender()
            .setCountry("DE")
            .setBidDeadline(LocalDateTime.now().minusDays(100))
            .setPublications(Arrays.asList(
                    new Publication().setFormType(PublicationFormType.CONTRACT_NOTICE),
                    new Publication().setFormType(PublicationFormType.CONTRACT_NOTICE)
                            .setPublicationDate(LocalDate.now())));

    private final AdvertisementPeriodIndicatorPlugin plugin = new AdvertisementPeriodIndicatorPlugin();

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
        assertNull(plugin.evaulate(tender3));
        assertNull(plugin.evaulate(tender6));
        assertThrown(() -> plugin.evaulate(tender7)).isInstanceOf(
                MisconfigurationException.class);

    }

    /**
     * Test of positive result.
     */
    @Test
    public void okTest() {
        assertEquals(plugin.evaulate(tender2).getType(),
                TenderIndicatorType.CORRUPTION_ADVERTISEMENT_PERIOD.name());

        assertEquals(plugin.evaulate(tender4).getType(),
                TenderIndicatorType.CORRUPTION_ADVERTISEMENT_PERIOD.name());

        assertEquals(plugin.evaulate(tender5).getType(),
                TenderIndicatorType.CORRUPTION_ADVERTISEMENT_PERIOD.name());
    }

    /**
     * Test of correct plugin type.
     */
    @Test
    public void getTypeTest() {
        assertEquals(plugin.getType(),
                TenderIndicatorType.CORRUPTION_ADVERTISEMENT_PERIOD.name());
    }
}
