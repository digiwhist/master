package eu.dl.worker.indicator.plugin;

import eu.dl.core.config.Config;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.indicator.IndicatorStatus;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import org.junit.Before;
import org.junit.Test;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Arrays;

import static org.junit.Assert.assertEquals;

/**
 * Test of lot decision period indicator plugin.
 *
 * @author Tomas Mrazek
 */
public final class LotDecisionPeriodIndicatorPluginTest {

    private final LotDecisionPeriodIndicatorPlugin plugin = new LotDecisionPeriodIndicatorPlugin();

    /**
     * Test initialization.
     */
    @Before
    public void init() {
        Config.getInstance().addConfigFile("unit_test");
    }

    /**
     * Test of undefined indicators.
     */
    @Test
    public void undefinedIndicatorTest() {
        MasterTender tender = new MasterTender()
            .setCountry("CZ")
            .setBidDeadline(LocalDateTime.now())
            .addLot(new MasterTenderLot());

        assertEquals(plugin.evaluate(tender.getLots().get(0), tender).getStatus(), IndicatorStatus.UNDEFINED);
    }

    /**
     * Test of insufficient indicators.
     */
    @Test
    public void insufficientIndicatorTest() {
        assertEquals(plugin.evaluate(new MasterTenderLot(), new MasterTender()).getStatus(), IndicatorStatus.INSUFFICIENT_DATA);

        assertEquals(plugin.evaluate(new MasterTenderLot(), null).getStatus(), IndicatorStatus.INSUFFICIENT_DATA);

        MasterTender tender = new MasterTender()
            .setCountry("CZ")
            .setBidDeadline(LocalDateTime.now().minusDays(100))
            .setPublications(Arrays.asList(new Publication().setFormType(PublicationFormType.CONTRACT_AWARD).setIsIncluded(true)))
            .addLot(new MasterTenderLot());
        assertEquals(plugin.evaluate(tender.getLots().get(0), tender).getStatus(), IndicatorStatus.INSUFFICIENT_DATA);

        tender = new MasterTender()
            .setCountry("CZ")
            .setBidDeadline(LocalDateTime.now().minusDays(100))
            .addLot(new MasterTenderLot().setAwardDecisionDate(LocalDate.now()));
        assertEquals(plugin.evaluate(tender.getLots().get(0), tender).getStatus(), IndicatorStatus.INSUFFICIENT_DATA);
    }

    /**
     * Test of calculated indicators.
     */
    @Test
    public void calculatedIndicatorTest() {
        assertEquals(plugin.evaluate(null, new MasterTender().setCountry("CZ")).getStatus(), IndicatorStatus.CALCULATED);

        MasterTender tender = new MasterTender()
            .setCountry("CZ")
            .setBidDeadline(LocalDateTime.now())
            .addLot(new MasterTenderLot().setAwardDecisionDate(LocalDate.now().minusDays(100)));
        assertEquals(plugin.evaluate(tender.getLots().get(0), tender).getStatus(), IndicatorStatus.CALCULATED);
    }
}
