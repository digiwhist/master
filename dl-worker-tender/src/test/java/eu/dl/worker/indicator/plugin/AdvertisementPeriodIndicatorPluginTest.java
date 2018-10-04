package eu.dl.worker.indicator.plugin;

import eu.dl.core.config.Config;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.indicator.IndicatorStatus;
import eu.dl.dataaccess.dto.master.MasterTender;
import org.junit.Before;
import org.junit.Test;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Arrays;

import static org.junit.Assert.assertEquals;

/**
 * Test of length of advertisement period indicator plugin.
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
            .setCountry("BE")
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
            .setCountry("BE")
            .setPublications(Arrays.asList(
                    new Publication().setFormType(PublicationFormType.CONTRACT_NOTICE),
                    new Publication().setFormType(PublicationFormType.CONTRACT_NOTICE)
                            .setPublicationDate(LocalDate.MAX)));

    private final MasterTender tender5 = new MasterTender()
            .setCountry("BE")
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
                    new Publication().setFormType(PublicationFormType.CONTRACT_AWARD)
                            .setPublicationDate(LocalDate.now())));

    private final AdvertisementPeriodIndicatorPlugin plugin = new AdvertisementPeriodIndicatorPlugin();

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
        assertEquals(plugin.evaluate(tender6).getStatus(), IndicatorStatus.INSUFFICIENT_DATA);
        assertEquals(plugin.evaluate(tender7).getStatus(), IndicatorStatus.INSUFFICIENT_DATA);
    }

    /**
     * Test of calculated indicators.
     */
    @Test
    public void calculatedIndicatorTest() {
        assertEquals(plugin.evaluate(tender2).getStatus(), IndicatorStatus.CALCULATED);
        assertEquals(plugin.evaluate(tender2).getValue(), new Double(0d));

        assertEquals(plugin.evaluate(tender3).getStatus(), IndicatorStatus.CALCULATED);
        assertEquals(plugin.evaluate(tender3).getValue(), new Double(100d));

        assertEquals(plugin.evaluate(tender4).getStatus(), IndicatorStatus.CALCULATED);
        assertEquals(plugin.evaluate(tender4).getValue(), new Double(0d));

        assertEquals(plugin.evaluate(tender5).getStatus(), IndicatorStatus.CALCULATED);
        assertEquals(plugin.evaluate(tender5).getValue(), new Double(100d));
    }
}
