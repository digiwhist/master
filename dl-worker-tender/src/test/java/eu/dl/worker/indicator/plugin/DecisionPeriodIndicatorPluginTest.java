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
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;

/**
 * Test of electronic auction indicator plugin.
 *
 * @author Jakub Krafka
 */
public final class DecisionPeriodIndicatorPluginTest {

    private final MasterTender nullTender = new MasterTender()
            .setLots(Arrays.asList(new MasterTenderLot()));

    private final MasterTender tender1 = new MasterTender()
            .setLots(Arrays.asList(new MasterTenderLot()));

    private final MasterTender tender2 = new MasterTender()
            .setCountry("CZ")
            .setLots(Arrays.asList(new MasterTenderLot()));

    private final MasterTender tender3 = new MasterTender()
            .setCountry("SK")
            .setLots(Arrays.asList(new MasterTenderLot()));

    private final MasterTender tender4 = new MasterTender()
            .setCountry("CZ")
            .setLots(Arrays.asList(new MasterTenderLot()));

    private final MasterTender tender5 = new MasterTender()
            .setCountry("CZ")
            .setBidDeadline(LocalDateTime.now())
            .setLots(Arrays.asList(new MasterTenderLot().setAwardDecisionDate(LocalDate.now())));

    private final MasterTender tender6 = new MasterTender()
            .setCountry("CZ")
            .setBidDeadline(LocalDateTime.now().minusDays(100))
            .setLots(Arrays.asList(new MasterTenderLot().setAwardDecisionDate(LocalDate.now())));

    private final MasterTender tender7 = new MasterTender()
            .setCountry("DE")
            .setBidDeadline(LocalDateTime.now().minusDays(100))
            .setLots(Arrays.asList(new MasterTenderLot().setAwardDecisionDate(LocalDate.now())));

    private final DecisionPeriodIndicatorPlugin plugin = new DecisionPeriodIndicatorPlugin();

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
                TenderIndicatorType.CORRUPTION_DECISION_PERIOD.name());

        assertEquals(plugin.evaulate(tender4).getType(),
                TenderIndicatorType.CORRUPTION_DECISION_PERIOD.name());

        assertEquals(plugin.evaulate(tender5).getType(),
                TenderIndicatorType.CORRUPTION_DECISION_PERIOD.name());
    }

    /**
     * Test of correct plugin type.
     */
    @Test
    public void getTypeTest() {
        assertEquals(plugin.getType(),
                TenderIndicatorType.CORRUPTION_DECISION_PERIOD.name());
    }
}
