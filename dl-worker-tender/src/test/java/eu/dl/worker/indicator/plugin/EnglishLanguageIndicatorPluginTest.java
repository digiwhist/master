package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.MasterTender;
import org.junit.Test;

import java.util.Arrays;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

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

    private final EnglishLanguageIndicatorPlugin plugin = new EnglishLanguageIndicatorPlugin();

    /**
     * Test of correct tender address.
     */
    @Test
    public void noIndicatorTest() {
        assertNull(plugin.evaulate(null));
        assertNull(plugin.evaulate(nullTender));
        assertNull(plugin.evaulate(tender2));
    }

    /**
     * Test of positive result.
     */
    @Test
    public void okTest() {
        assertEquals(plugin.evaulate(tender1).getType(),
                TenderIndicatorType.ADMINISTRATIVE_ENGLISH_AS_FOREIGN_LANGUAGE.name());
    }

    /**
     * Test of correct plugin type.
     */
    @Test
    public void getTypeTest() {
        assertEquals(plugin.getType(),
                TenderIndicatorType.ADMINISTRATIVE_ENGLISH_AS_FOREIGN_LANGUAGE.name());
    }
}
