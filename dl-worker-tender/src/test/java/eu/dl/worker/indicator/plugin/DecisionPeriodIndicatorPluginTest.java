package eu.dl.worker.indicator.plugin;

import eu.dl.core.config.Config;
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
 * Test of electronic auction indicator plugin.
 *
 * @author Jakub Krafka
 */
public final class DecisionPeriodIndicatorPluginTest {

    private final MasterTender nullTender = new MasterTender();

    private final MasterTender tender1 = new MasterTender()
            .setLots(Arrays.asList(new MasterTenderLot()));

    private final MasterTender tender2 = new MasterTender()
            .setCountry("CZ")
            .setLots(Arrays.asList(new MasterTenderLot()));

    private final MasterTender tender3 = new MasterTender()
            .setCountry("SK")
            .setLots(Arrays.asList(new MasterTenderLot()));

    private final MasterTender tender4 = new MasterTender()
            .setCountry("DE")
            .setLots(Arrays.asList(new MasterTenderLot()));

    private final MasterTender tender5 = new MasterTender()
            .setCountry("CZ")
            .setBidDeadline(LocalDateTime.now())
            .setLots(Arrays.asList(new MasterTenderLot()));

    private final MasterTender tender6 = new MasterTender()
            .setCountry("CZ")
            .setBidDeadline(LocalDateTime.now())
            .setLots(Arrays.asList(new MasterTenderLot()
                    .setAwardDecisionDate(LocalDate.now())));

    private final MasterTender tender7 = new MasterTender()
            .setCountry("CZ")
            .setBidDeadline(LocalDateTime.now().minusDays(100))
            .setLots(Arrays.asList(new MasterTenderLot()
                    .setAwardDecisionDate(LocalDate.now())));

    private final MasterTender tender8 = new MasterTender()
            .setCountry("DE")
            .setBidDeadline(LocalDateTime.now().minusDays(100))
            .setLots(Arrays.asList(new MasterTenderLot()
                    .setAwardDecisionDate(LocalDate.now())));

    private final MasterTender tender9 = new MasterTender()
            .setCountry("DE")
            .setBidDeadline(LocalDateTime.now().minusDays(100))
            .setLots(Arrays.asList(
                    new MasterTenderLot()
                            .setAwardDecisionDate(LocalDate.now()),
                    new MasterTenderLot()
                            .setAwardDecisionDate(LocalDate.now().minusDays(80))));

    private final DecisionPeriodIndicatorPlugin plugin = new DecisionPeriodIndicatorPlugin();

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
        assertEquals(plugin.evaluate(tender5).getStatus(), IndicatorStatus.UNDEFINED);
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
        assertEquals(plugin.evaluate(tender3).getValue(), new Double(100d));

        assertEquals(plugin.evaluate(tender4).getStatus(), IndicatorStatus.CALCULATED);
        assertEquals(plugin.evaluate(tender4).getValue(), new Double(0d));

        assertEquals(plugin.evaluate(tender6).getStatus(), IndicatorStatus.CALCULATED);
        assertEquals(plugin.evaluate(tender6).getValue(), new Double(0d));

        assertEquals(plugin.evaluate(tender7).getStatus(), IndicatorStatus.CALCULATED);
        assertEquals(plugin.evaluate(tender7).getValue(), new Double(0d));

        assertEquals(plugin.evaluate(tender8).getStatus(), IndicatorStatus.CALCULATED);
        assertEquals(plugin.evaluate(tender8).getValue(), new Double(100d));

        assertEquals(plugin.evaluate(tender9).getStatus(), IndicatorStatus.CALCULATED);
        assertEquals(plugin.evaluate(tender9).getValue(), new Double(0d));
    }
}
