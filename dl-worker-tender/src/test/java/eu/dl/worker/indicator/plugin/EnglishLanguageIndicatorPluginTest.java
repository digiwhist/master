package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dto.indicator.IndicatorStatus;
import eu.dl.dataaccess.dto.master.MasterTender;
import org.junit.Test;

import java.util.Arrays;

import static org.junit.Assert.assertEquals;

/**
 * Test of english as foreign language indicator.
 *
 * @author Jakub Krafka
 */
public final class EnglishLanguageIndicatorPluginTest {

    private final MasterTender nullTender = new MasterTender();

    private final MasterTender tender1 = new MasterTender()
            .setEligibleBidLanguages(Arrays.asList("en", "cs", "de"));

    private final MasterTender tender2 = new MasterTender()
            .setEligibleBidLanguages(Arrays.asList("fsdf", "cs", "de"));

    private final MasterTender tender3 = new MasterTender()
            .setCountry("IE")
            .setEligibleBidLanguages(Arrays.asList("en", "cs", "de"));

    private final MasterTender tender4 = new MasterTender()
            .setCountry("CZ")
            .setEligibleBidLanguages(Arrays.asList("fsdf", "cs", "de"));

    private final MasterTender tender5 = new MasterTender()
            .setCountry("CZ")
            .setEligibleBidLanguages(Arrays.asList("en", "cs", "de"));

    private final EnglishLanguageIndicatorPlugin plugin = new EnglishLanguageIndicatorPlugin();

    /**
     * Test of undefined indicators.
     */
    @Test
    public void undefinedIndicatorTest() {
        assertEquals(plugin.evaluate(tender3).getStatus(), IndicatorStatus.UNDEFINED);
    }

    /**
     * Test of insufficient indicators.
     */
    @Test
    public void insufficientIndicatorTest() {
        assertEquals(plugin.evaluate(null).getStatus(), IndicatorStatus.INSUFFICIENT_DATA);
        assertEquals(plugin.evaluate(nullTender).getStatus(), IndicatorStatus.INSUFFICIENT_DATA);
        assertEquals(plugin.evaluate(tender1).getStatus(), IndicatorStatus.INSUFFICIENT_DATA);
        assertEquals(plugin.evaluate(tender2).getStatus(), IndicatorStatus.INSUFFICIENT_DATA);
    }

    /**
     * Test of calculated indicators.
     */
    @Test
    public void calculatedIndicatorTest() {
        assertEquals(plugin.evaluate(tender4).getStatus(), IndicatorStatus.CALCULATED);
        assertEquals(plugin.evaluate(tender4).getValue(), new Double(0d));

        assertEquals(plugin.evaluate(tender5).getStatus(), IndicatorStatus.CALCULATED);
        assertEquals(plugin.evaluate(tender5).getValue(), new Double(100d));
    }

}
