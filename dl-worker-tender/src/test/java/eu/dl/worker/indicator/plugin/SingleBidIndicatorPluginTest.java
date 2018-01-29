package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.indicator.Indicator;
import eu.dl.dataaccess.dto.indicator.IndicatorStatus;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import org.junit.Test;

import java.util.Arrays;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

/**
 * Test of electronic auction indicator plugin.
 *
 * @author Jakub Krafka
 */
public final class SingleBidIndicatorPluginTest {

    private final MasterTender nullTender = new MasterTender()
            .setPublications(Arrays.asList(new Publication()
                    .setIsIncluded(true).setFormType(PublicationFormType.CONTRACT_AWARD)))
            .setLots(Arrays.asList(new MasterTenderLot()));

    private final MasterTender tender1 = new MasterTender()
            .setPublications(Arrays.asList(new Publication()
                    .setIsIncluded(true).setFormType(PublicationFormType.CONTRACT_AWARD)))
            .setLots(Arrays.asList(new MasterTenderLot().setValidBidsCount(null)));

    private final MasterTender tender2 = new MasterTender()
            .setPublications(Arrays.asList(new Publication()
                    .setIsIncluded(true).setFormType(PublicationFormType.CONTRACT_AWARD)))
            .setLots(Arrays.asList(new MasterTenderLot()
                    .setValidBidsCount(2)));

    private final MasterTender tender3 = new MasterTender()
            .setPublications(Arrays.asList(new Publication()
                    .setIsIncluded(true).setFormType(PublicationFormType.CONTRACT_AWARD)))
            .setLots(Arrays.asList(new MasterTenderLot()
                    .setValidBidsCount(1)));

    private final MasterTender tender4 = new MasterTender()
            .setLots(Arrays.asList(new MasterTenderLot()
                    .setBidsCount(0)));

    private final MasterTender tender5 = new MasterTender()
            .setPublications(Arrays.asList(new Publication()
                    .setIsIncluded(true).setFormType(PublicationFormType.CONTRACT_AWARD)))
            .setLots(Arrays.asList(new MasterTenderLot()
                    .setBidsCount(1)));

    private final SingleBidIndicatorPlugin plugin = new SingleBidIndicatorPlugin();

    /**
     * Test of correct tender address.
     */
    @Test
    public void insufficientDataIndicatorTest() {
        Indicator indicator = plugin.evaluate(nullTender);
        assertEquals(TenderIndicatorType.INTEGRITY_SINGLE_BID.name(), indicator.getType());
        assertEquals(IndicatorStatus.INSUFFICIENT_DATA, indicator.getStatus());
        assertNull(indicator.getValue());

        indicator = plugin.evaluate(tender1);
        assertEquals(TenderIndicatorType.INTEGRITY_SINGLE_BID.name(), indicator.getType());
        assertEquals(IndicatorStatus.INSUFFICIENT_DATA, indicator.getStatus());
        assertNull(indicator.getValue());
    }

    /**
     * Test of undefined status.
     */
    @Test
    public void undefinedTest() {
        Indicator indicator = plugin.evaluate(tender4);
        assertEquals(TenderIndicatorType.INTEGRITY_SINGLE_BID.name(), indicator.getType());
        assertEquals(IndicatorStatus.UNDEFINED, indicator.getStatus());
        assertNull(indicator.getValue());
    }

    /**
     * Test of positive result.
     */
    @Test
    public void calculatedTest() {
        Indicator indicator = plugin.evaluate(tender3);
        assertEquals(TenderIndicatorType.INTEGRITY_SINGLE_BID.name(), indicator.getType());
        assertEquals(IndicatorStatus.CALCULATED, indicator.getStatus());
        assertEquals(new Double(0), indicator.getValue());

        indicator = plugin.evaluate(tender5);
        assertEquals(TenderIndicatorType.INTEGRITY_SINGLE_BID.name(), indicator.getType());
        assertEquals(IndicatorStatus.CALCULATED, indicator.getStatus());
        assertEquals(new Double(0), indicator.getValue());

        indicator = plugin.evaluate(tender2);
        assertEquals(TenderIndicatorType.INTEGRITY_SINGLE_BID.name(), indicator.getType());
        assertEquals(IndicatorStatus.CALCULATED, indicator.getStatus());
        assertEquals(new Double(100), indicator.getValue());
    }

    /**
     * Test of correct plugin type.
     */
    @Test
    public void getTypeTest() {
        assertEquals(plugin.getType(),
                TenderIndicatorType.INTEGRITY_SINGLE_BID.name());
    }
}
