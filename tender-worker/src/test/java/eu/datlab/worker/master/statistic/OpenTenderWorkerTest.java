package eu.datlab.worker.master.statistic;

import eu.dl.dataaccess.dto.generic.Price;
import eu.dl.dataaccess.dto.master.MasterBid;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import java.math.BigDecimal;
import java.util.Arrays;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

/**
 *
 * @author Tomas Mrazek
 */
public final class OpenTenderWorkerTest {
    /**
     * Test of {@link OpenTenderWorker#getPrice(eu.dl.dataaccess.dto.master.MasterTender)} method.
     */
    @Test
    public void getPriceTest() {
        MasterTender tender = new MasterTender()
            .setFinalPrice(new Price().setNetAmountEur(BigDecimal.valueOf(10)))
            .setEstimatedPrice(new Price().setNetAmountEur(BigDecimal.valueOf(20)))
            .addLot(new MasterTenderLot()
                .setBids(Arrays.asList(
                    new MasterBid()
                        .setIsWinning(Boolean.TRUE)
                        .setPrice(new Price().setNetAmountEur(BigDecimal.valueOf(30))),
                    new MasterBid()
                        .setIsWinning(Boolean.TRUE)
                        .setPrice(new Price().setNetAmountEur(BigDecimal.valueOf(40)))))
                .setEstimatedPrice(new Price().setNetAmountEur(BigDecimal.valueOf(50))))
            .addLot(new MasterTenderLot()
                .setBids(Arrays.asList(
                    new MasterBid()
                        .setIsWinning(Boolean.TRUE)
                        .setPrice(new Price().setNetAmountEur(BigDecimal.valueOf(60)))))
                .setEstimatedPrice(new Price().setNetAmountEur(BigDecimal.valueOf(70))));

        
        
        BigDecimal price = OpenTenderWorker.getPrice(tender);
        // final price
        assertEquals(10L, price.longValue());


        tender.getFinalPrice().setNetAmountEur(null);
        price = OpenTenderWorker.getPrice(tender);
        // estimated price
        assertEquals(20L, price.longValue());


        tender.getEstimatedPrice().setNetAmountEur(null);
        price = OpenTenderWorker.getPrice(tender);
        // sum of prices of winning bids of all lots
        assertEquals(130L, price.longValue());


        tender.getLots().get(0).setBids(null);
        tender.getLots().get(1).setBids(null);
        price = OpenTenderWorker.getPrice(tender);
        // sum of estimated prices of all lots
        assertEquals(120L, price.longValue());


        tender.getLots().get(0).setEstimatedPrice(null);
        tender.getLots().get(1).setEstimatedPrice(null);
        price = OpenTenderWorker.getPrice(tender);
        // no suitable price was found
        assertNull(price);
    }

    /**
     * Test of {@link OpenTenderWorker#isOpenTender(eu.dl.dataaccess.dto.master.MasterTender)} method.
     */
    @Test
    public void isOpenTenderTest() {
        MasterTender tender = new MasterTender();

        // TED form from supported country is open tender
        tender.setCountry(OpenTenderWorker.COUNTRIES.get(0));
        tender.setCreatedBy(OpenTenderWorker.WORKERS_EU.get(0));
        assertTrue(OpenTenderWorker.isOpenTender(tender));

        // Georgina form is open tender
        tender = new MasterTender();
        tender.setCreatedBy(OpenTenderWorker.WORKER_GE);
        assertTrue(OpenTenderWorker.isOpenTender(tender));


        // Polish form is open tender
        tender = new MasterTender();
        tender.setCreatedBy(OpenTenderWorker.WORKER_PL);
        assertTrue(OpenTenderWorker.isOpenTender(tender));


        // TED form with price over OpenTenderWorker#THRESHOLD is opentender
        tender = new MasterTender();
        tender.setCountry("XX"); // not in OpenTenderWorker#COUNTRIES
        tender.setCreatedBy(OpenTenderWorker.WORKERS_EU.get(0));
        tender.setFinalPrice(new Price().setNetAmountEur(BigDecimal.valueOf(136000)));
        assertTrue(OpenTenderWorker.isOpenTender(tender));

        // TED form with price under or equal to the OpenTenderWorker#THRESHOLD is not opentender
        tender.setFinalPrice(new Price().setNetAmountEur(BigDecimal.valueOf(10)));
        assertFalse(OpenTenderWorker.isOpenTender(tender));


        // Form with price under threshold is opentender
        tender = new MasterTender();
        tender.setCreatedBy("XX"); // not EU, PL or GE worker
        tender.setFinalPrice(new Price().setNetAmountEur(BigDecimal.valueOf(10)));
        assertTrue(OpenTenderWorker.isOpenTender(tender));


        // Form with null price (calculated by OpenTenderWorker#getPrice())
        tender = new MasterTender();
        tender.setCreatedBy("XX");
        assertTrue(OpenTenderWorker.isOpenTender(tender));
        

        assertFalse(OpenTenderWorker.isOpenTender(new MasterTender()));
        assertFalse(OpenTenderWorker.isOpenTender(null));        
    }
}
