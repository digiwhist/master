package eu.dl.worker.indicator.plugin;

import eu.dl.core.config.Config;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.generic.Address;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.indicator.IndicatorStatus;
import eu.dl.dataaccess.dto.master.MasterBid;
import eu.dl.dataaccess.dto.master.MasterBody;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import org.junit.Before;
import org.junit.Test;

import java.time.LocalDate;
import java.util.Arrays;

import static org.junit.Assert.assertEquals;

/**
 * Test of tax haven indicator plugin.
 *
 * @author Jakub Krafka
 */
public final class TaxHavenIndicatorPluginTest {

    private final MasterTender nullTender = new MasterTender()
            .setLots(Arrays.asList(new MasterTenderLot()));

    private final MasterTender tender1 = new MasterTender()
            .setLots(Arrays.asList(new MasterTenderLot()));
    
	private final MasterTender tender2 = new MasterTender()
			.setLots(Arrays.asList(
			        new MasterTenderLot()
                            .setBids(Arrays.asList(
                                    new MasterBid()
                                            .setBidders(Arrays.asList(
                                                    new MasterBody()
                                                            .setAddress(new Address()
                                                                    .setCountry("CZ"))))
                                            .setIsWinning(true)))))
            .setPublications(Arrays.asList(new Publication()
                    .setPublicationDate(LocalDate.of(2017, 1, 1))
                    .setFormType(PublicationFormType.CONTRACT_AWARD)));

	private final MasterTender tender3 = new MasterTender()
			.setLots(Arrays.asList(
			        new MasterTenderLot()
                            .setBids(Arrays.asList(
                                    new MasterBid()
                                            .setBidders(Arrays.asList(
                                                    new MasterBody()
                                                            .setAddress(new Address()
                                                                    .setCountry("CZ"))))
                                            .setIsWinning(true)))))
            .setPublications(Arrays.asList(new Publication()
                    .setPublicationDate(LocalDate.of(2017, 1, 1))
                    .setFormType(PublicationFormType.CONTRACT_NOTICE)));

    private final TaxHavenIndicatorPlugin plugin = new TaxHavenIndicatorPlugin();

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
    }

    /**
     * Test of calculated indicators.
     */
    @Test
    public void calculatedIndicatorTest() {
        assertEquals(plugin.evaluate(tender2).getStatus(), IndicatorStatus.CALCULATED);
        assertEquals(plugin.evaluate(tender2).getValue(), new Double(0d));
    }

}
