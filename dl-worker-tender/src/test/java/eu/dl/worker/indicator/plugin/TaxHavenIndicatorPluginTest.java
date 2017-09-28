package eu.dl.worker.indicator.plugin;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.time.LocalDate;
import java.util.Arrays;

import org.junit.Before;
import org.junit.Test;

import eu.dl.core.config.Config;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.generic.Address;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.MasterBid;
import eu.dl.dataaccess.dto.master.MasterBody;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;

/**
 * Test of electronic auction indicator plugin.
 *
 * @author Jakub Krafka
 */
public final class TaxHavenIndicatorPluginTest {

    private final MasterTender nullTender = new MasterTender()
            .setLots(Arrays.asList(new MasterTenderLot()));

    private final MasterTender tender1 = new MasterTender()
            .setLots(Arrays.asList(new MasterTenderLot()));
    
	private final MasterTender tender2 = new MasterTender()
			.setLots(
				Arrays.asList(
					new MasterTenderLot().setBids(
						Arrays.asList(
							new MasterBid().setBidders(
								Arrays.asList(
									new MasterBody().setAddress(
										new Address().setCountry("CZ")
									)
								)
							).setIsWinning(true)
						)
					)
				)
			).setPublications(
				Arrays.asList(
					new Publication()
						.setPublicationDate(LocalDate.of(2017, 1, 1))
						.setFormType(PublicationFormType.CONTRACT_AWARD
					)
				)
			);

    private final TaxHavenIndicatorPlugin plugin = new TaxHavenIndicatorPlugin();

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
    }

    /**
     * Test of positive result.
     */
    @Test
    public void okTest() {
        assertEquals(plugin.evaulate(tender2).getType(),
                TenderIndicatorType.CORRUPTION_TAX_HAVEN.name());
    }

    /**
     * Test of correct plugin type.
     */
    @Test
    public void getTypeTest() {
        assertEquals(plugin.getType(),
                TenderIndicatorType.CORRUPTION_TAX_HAVEN.name());
    }
}
