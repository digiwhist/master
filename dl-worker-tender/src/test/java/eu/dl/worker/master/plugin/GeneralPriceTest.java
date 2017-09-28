package eu.dl.worker.master.plugin;

import eu.dl.dataaccess.dto.generic.Price;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.dataaccess.dto.matched.MatchedTenderLot;
import eu.dl.dataaccess.dto.matched.StructuredLotId;
import org.junit.Test;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertTrue;

/**
 * Test of basic price mastering.
 *
 * @author Marek Mikes
 */
public final class GeneralPriceTest {
    private static final String TENDER1_ID = "tender1ID";
    private static final String TENDER2_ID = "tender2ID";
    private static final String TENDER3_ID = "tender3ID";

    private final MatchedTender tender1 = new MatchedTender()
            .setEstimatedPrice(new Price()
                    .setNetAmount(new BigDecimal(1500)))
            .addLot(new MatchedTenderLot()
                    .setEstimatedPrice(new Price()
                            .setNetAmount(new BigDecimal(600)))
                    .setStructuredId(new StructuredLotId()
                            .setTenderId(TENDER1_ID)))
            .addLot(new MatchedTenderLot()
                    .setEstimatedPrice(new Price()
                            .setNetAmount(new BigDecimal(900)))
                    .setStructuredId(new StructuredLotId()
                            .setTenderId(TENDER1_ID))
                    .setPublicationDate(LocalDate.now().minusDays(1)));
    private final MatchedTender tender2 = new MatchedTender()
            .setEstimatedPrice(new Price()
                    .setNetAmount(new BigDecimal(19000)));
    private final MatchedTender tender3 = new MatchedTender()
            .setEstimatedPrice(new Price()
                    .setNetAmount(new BigDecimal(1900)))
            .addLot(new MatchedTenderLot()
                    .setEstimatedPrice(new Price()
                            .setNetAmount(new BigDecimal(650)))
                    .setStructuredId(new StructuredLotId()
                            .setTenderId(TENDER3_ID)))
            .addLot(new MatchedTenderLot()
                    .setEstimatedPrice(new Price()
                            .setNetAmount(new BigDecimal(850)))
                    .setStructuredId(new StructuredLotId()
                            .setTenderId(TENDER3_ID))
                    .setPublicationDate(LocalDate.now()));

    /**
     * Default constructor to set tender IDs. The setter does not support fluent interface, so we call it here.
     */
    public GeneralPriceTest() {
        tender1.setId(TENDER1_ID);
        tender2.setId(TENDER2_ID);
        tender3.setId(TENDER3_ID);
    }

    /**
     * Test of correct price.
     */
    @Test
    public void correctPriceTest() {
        List<MatchedTender> allTenders = Arrays.asList(tender1, tender2, tender3);

        // tenders:
        GeneralPricePlugin generalPricePlugin = new GeneralPricePlugin(Arrays.asList("estimatedPrice"));
        MasterTender masterTender = new MasterTender();
        generalPricePlugin.master(allTenders, masterTender, allTenders);
        assertTrue(masterTender.getEstimatedPrice().equals(tender3.getEstimatedPrice()));

        // lots:
        GeneralPricePlugin generalLotPricePlugin = new GeneralPricePlugin(Arrays.asList("estimatedPrice"));
        MasterTenderLot masterLot = new MasterTenderLot();
        // modification dates are filled -> last price is chosen
        generalLotPricePlugin.master(Arrays.asList(tender1.getLots().get(0), tender3.getLots().get(0)), masterLot,
                Arrays.asList(tender1, tender3));
        assertTrue(masterLot.getEstimatedPrice().equals(tender3.getLots().get(0).getEstimatedPrice()));
        // modification dates are filled -> last price is chosen
        generalLotPricePlugin.master(Arrays.asList(tender3.getLots().get(0), tender1.getLots().get(0)), masterLot,
                Arrays.asList(tender1, tender3));
        assertTrue(masterLot.getEstimatedPrice().equals(tender3.getLots().get(0).getEstimatedPrice()));
        // modification dates are filled -> last price is chosen
        generalLotPricePlugin.master(Arrays.asList(tender1.getLots().get(1), tender3.getLots().get(1)), masterLot,
                Arrays.asList(tender1, tender3));
        assertTrue(masterLot.getEstimatedPrice().equals(tender3.getLots().get(1).getEstimatedPrice()));
        // modification dates are filled -> last price is chosen
        generalLotPricePlugin.master(Arrays.asList(tender3.getLots().get(1), tender1.getLots().get(1)), masterLot,
                Arrays.asList(tender1, tender3));
        assertTrue(masterLot.getEstimatedPrice().equals(tender3.getLots().get(1).getEstimatedPrice()));
    }
}
