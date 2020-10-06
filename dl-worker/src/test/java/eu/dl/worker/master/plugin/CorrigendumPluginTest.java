package eu.dl.worker.master.plugin;

import eu.dl.dataaccess.dto.codetables.CorrectionType;
import eu.dl.dataaccess.dto.generic.CPV;
import eu.dl.dataaccess.dto.generic.Corrigendum;
import eu.dl.dataaccess.dto.generic.Price;
import eu.dl.dataaccess.dto.master.MasterBid;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import eu.dl.worker.master.plugin.specific.CorrigendumPlugin;
import org.junit.Test;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Month;
import java.util.Arrays;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;

//import static org.junit.Assert.*;

/**
 * CorrigendumPlugin tests.
 *
 * @author Tomas Mrazek
 */
public class CorrigendumPluginTest {

    /**
     * Test of simple bid deadline correction.
     */
    @Test
    public final void pluginBidDeadlineTest() {
        CorrigendumPlugin plugin = new CorrigendumPlugin();

        MasterTender tender = new MasterTender()
                .setBidDeadline(LocalDateTime.of(2019, Month.JANUARY, 1, 0, 0))
                .setCorrections(Arrays.asList(new Corrigendum()
                        .setPublicationDate(LocalDate.now())
                        .setSectionNumber("IV.2.2")
                        .setOriginalDate(LocalDateTime.of(2019, Month.JANUARY, 1, 0, 0))
                        .setReplacementDate(LocalDateTime.of(2019, Month.FEBRUARY, 1, 0, 0)))
                );

        tender = plugin.master(null, tender, null);

        assertEquals(LocalDateTime.of(2019, Month.FEBRUARY, 1, 0, 0), tender.getBidDeadline());
        assertTrue(tender.getCorrections().get(0).getIsIncluded());


        tender = new MasterTender()
                .setBidDeadline(LocalDateTime.of(2019, Month.JANUARY, 1, 0, 0))
                .setCorrections(Arrays.asList(new Corrigendum()
                        .setPublicationDate(LocalDate.now())
                        .setSectionNumber("IV.2.2")
                        .setReplacementDate(LocalDateTime.of(2019, Month.FEBRUARY, 1, 0, 0)))
                );

        tender = plugin.master(null, tender, null);

        assertEquals(LocalDateTime.of(2019, Month.FEBRUARY, 1, 0, 0), tender.getBidDeadline());
        assertTrue(tender.getCorrections().get(0).getIsIncluded());


        tender = new MasterTender()
                .setBidDeadline(LocalDateTime.of(2019, Month.JANUARY, 1, 0, 0))
                .setCorrections(Arrays.asList(new Corrigendum()
                        .setPublicationDate(LocalDate.now())
                        .setSectionNumber("I.1.1")
                        .setReplacementDate(LocalDateTime.of(2019, Month.FEBRUARY, 1, 0, 0)))
                );

        tender = plugin.master(null, tender, null);

        assertEquals(LocalDateTime.of(2019, Month.JANUARY, 1, 0, 0), tender.getBidDeadline());
        assertFalse(tender.getCorrections().get(0).getIsIncluded());
    }

    /**
     * Test of sequence bid deadline corrections with missed dataTime.
     */
    @Test
    public final void sequenceCorrectionsPluginBidDeadlineTest() {
        CorrigendumPlugin plugin = new CorrigendumPlugin();
        MasterTender tender;

        tender = new MasterTender()
                .setBidDeadline(LocalDateTime.of(2019, Month.JANUARY, 1, 0, 0))
                .setCorrections(Arrays.asList(
                        new Corrigendum() // correction should be applied
                                .setPublicationDate(LocalDate.now().minusDays(5))
                                .setSectionNumber("IV.2.2")
                                .setOriginalDate(LocalDateTime.of(2019, Month.JANUARY, 1, 0, 0))
                                .setReplacementDate(LocalDateTime.of(2019, Month.FEBRUARY, 2, 10, 0)),
                        new Corrigendum()  // correction should be applied
                                .setPublicationDate(LocalDate.now().minusDays(4))
                                .setSectionNumber("IV.2.2")
                                // original date is previous value of bidDeadline before the first correction.
                                .setOriginalDate(LocalDateTime.of(2019, Month.JANUARY, 1, 0, 0))
                                // time is not set, time from the previous value should be used.
                                .setReplacementDate(LocalDateTime.of(2019, Month.FEBRUARY, 3, 0, 0)),
                        new Corrigendum()  // correction shouldn't be applied
                                .setPublicationDate(LocalDate.now().minusDays(3))
                                .setSectionNumber("IV.2.2")
                                // original date does not match any of the previous values of bidDeadline.
                                .setOriginalDate(LocalDateTime.of(2019, Month.JANUARY, 1, 23, 59))
                                // time is set.
                                .setReplacementDate(LocalDateTime.of(2019, Month.FEBRUARY, 2, 20, 0)),
                        new Corrigendum()  // correction should be applied
                                .setPublicationDate(LocalDate.now().minusDays(2))
                                .setSectionNumber("IV.2.2")
                                // original date does not matches one of the previous values of bidDeadline.
                                .setOriginalDate(LocalDateTime.of(2019, Month.FEBRUARY, 2, 10, 0))
                                // time is set.
                                .setReplacementDate(LocalDateTime.of(2019, Month.FEBRUARY, 2, 20, 0)),
                        new Corrigendum()  // correction should be applied.
                                .setPublicationDate(LocalDate.now().minusDays(1))
                                .setSectionNumber("IV.2.2")
                                // original date is equal to the actual bidDeadline value, time is missed.
                                .setOriginalDate(LocalDateTime.of(2019, Month.FEBRUARY, 2, 0, 0))
                                // time is not set, the previous value should be used.
                                .setReplacementDate(LocalDateTime.of(2019, Month.MARCH, 3, 0, 0))
                ));

        tender = plugin.master(null, tender, null);

        assertEquals(LocalDateTime.of(2019, Month.MARCH, 3, 20, 0), tender.getBidDeadline());
        assertTrue(tender.getCorrections().get(0).getIsIncluded());
        assertTrue(tender.getCorrections().get(1).getIsIncluded());
        assertFalse(tender.getCorrections().get(2).getIsIncluded());
        assertTrue(tender.getCorrections().get(3).getIsIncluded());
        assertTrue(tender.getCorrections().get(4).getIsIncluded());
        assertEquals(CorrectionType.BID_DEADLINE, tender.getCorrections().get(0).getType());
        assertEquals(CorrectionType.BID_DEADLINE, tender.getCorrections().get(1).getType());
        assertNull(tender.getCorrections().get(2).getType());
        assertEquals(CorrectionType.BID_DEADLINE, tender.getCorrections().get(3).getType());
        assertEquals(CorrectionType.BID_DEADLINE, tender.getCorrections().get(4).getType());
    }

    /**
     * Test of CPVs correction.
     */
    @Test
    public final void pluginCPVSTest() {
        CorrigendumPlugin plugin = new CorrigendumPlugin();
        MasterTender tender;
        CPV cpv = new CPV(), originCpv = new CPV(), replacementCpv = new CPV();

        cpv.setCode("1234567");
        originCpv.setCode("1234567");
        replacementCpv.setCode("7654321");

        // correct replacement.
        tender = new MasterTender()
                .setCpvs(Arrays.asList(cpv))
                .setCorrections(Arrays.asList(new Corrigendum()
                        .setPublicationDate(LocalDate.now())
                        .setSectionNumber("II.1.2")
                        .setOriginalCpvs(Arrays.asList(originCpv))
                        .setReplacementCpvs(Arrays.asList(replacementCpv)))
                );

        tender = plugin.master(null, tender, null);

        assertEquals(Arrays.asList(replacementCpv), tender.getCpvs());
        assertTrue(tender.getCorrections().get(0).getIsIncluded());

        // empty origin CPVs.
        tender = new MasterTender()
                .setCpvs(Arrays.asList(cpv))
                .setCorrections(Arrays.asList(new Corrigendum()
                        .setPublicationDate(LocalDate.now())
                        .setSectionNumber("II.1.6")
                        .setOriginalCpvs(null)
                        .setReplacementCpvs(Arrays.asList(replacementCpv)))
                );

        tender = plugin.master(null, tender, null);

        assertEquals(Arrays.asList(replacementCpv), tender.getCpvs());
        assertTrue(tender.getCorrections().get(0).getIsIncluded());

        // actual CPVs and origin CPVs are not equal.
        originCpv.setCode("1000000");
        tender = new MasterTender()
                .setCpvs(Arrays.asList(cpv))
                .setCorrections(Arrays.asList(new Corrigendum()
                        .setPublicationDate(LocalDate.now())
                        .setSectionNumber("II.1.6")
                        .setOriginalCpvs(Arrays.asList(originCpv))
                        .setReplacementCpvs(Arrays.asList(replacementCpv)))
                );

        tender = plugin.master(null, tender, null);

        assertEquals(Arrays.asList(cpv), tender.getCpvs());
        assertFalse(tender.getCorrections().get(0).getIsIncluded());

        // Replacing with null.
        originCpv.setCode(cpv.getCode());
        tender = new MasterTender()
                .setCpvs(Arrays.asList(cpv))
                .setCorrections(Arrays.asList(new Corrigendum()
                        .setPublicationDate(LocalDate.now())
                        .setSectionNumber("II.1.6")
                        .setOriginalCpvs(Arrays.asList(originCpv))
                        .setReplacementCpvs(null))
                );

        tender = plugin.master(null, tender, null);

        assertEquals(Arrays.asList(cpv), tender.getCpvs());
        assertNull(tender.getCorrections().get(0).getIsIncluded());
    }

    /**
     * Test of price corrections.
     */
    @Test
    public final void pluginPriceTest() {
        CorrigendumPlugin plugin = new CorrigendumPlugin();

        // tenderOrigin != null, correctionOrigin != null
        // fix tender.finalPrice and lots[0].bids[0].price
        MasterTender tender = new MasterTender()
            .setFinalPrice(new Price().setNetAmount(BigDecimal.ONE))
            .addLot(new MasterTenderLot()
                .setBids(Arrays.asList(new MasterBid().setPrice(new Price().setNetAmount(BigDecimal.ONE)))))
            .addLot(new MasterTenderLot()
                .setBids(Arrays.asList(new MasterBid().setPrice(new Price().setNetAmount(BigDecimal.ZERO)))))
            .setCorrections(Arrays.asList(new Corrigendum()
                .setPublicationDate(LocalDate.now())
                .setSectionNumber("V.2.4")
                .setOriginalValue(new Price().setNetAmount(BigDecimal.ONE))
                .setReplacementValue(new Price().setNetAmount(BigDecimal.TEN)))
            );

        tender = plugin.master(null, tender, null);

        assertEquals(BigDecimal.TEN, tender.getFinalPrice().getNetAmount());
        assertEquals(BigDecimal.TEN, tender.getLots().get(0).getBids().get(0).getPrice().getNetAmount());
        assertEquals(BigDecimal.ZERO, tender.getLots().get(1).getBids().get(0).getPrice().getNetAmount());

        // tenderOrigin != null, correctionOrigin == null, single lot
        // fix only tender.finalPrice
        tender = new MasterTender()
            .setFinalPrice(new Price().setNetAmount(BigDecimal.ONE))
            .addLot(new MasterTenderLot()
                .setBids(Arrays.asList(new MasterBid().setPrice(new Price().setNetAmount(BigDecimal.ONE)))))
            .setCorrections(Arrays.asList(new Corrigendum()
                .setPublicationDate(LocalDate.now())
                .setSectionNumber("V.2.4")
                .setReplacementValue(new Price().setNetAmount(BigDecimal.TEN)))
            );

        tender = plugin.master(null, tender, null);

        assertEquals(BigDecimal.TEN, tender.getFinalPrice().getNetAmount());
        assertEquals(BigDecimal.ONE, tender.getLots().get(0).getBids().get(0).getPrice().getNetAmount());

        // tenderOrigin == null, correctionOrigin != null, multi lot
        // no fix
        tender = new MasterTender()
            .setFinalPrice(new Price().setNetAmount(BigDecimal.ONE))
            .addLot(new MasterTenderLot()
                .setBids(Arrays.asList(new MasterBid().setPrice(new Price().setNetAmount(BigDecimal.ONE)))))
            .addLot(new MasterTenderLot()
                .setBids(Arrays.asList(new MasterBid())))
            .setCorrections(Arrays.asList(new Corrigendum()
                .setPublicationDate(LocalDate.now())
                .setSectionNumber("V.2.4")
                .setReplacementValue(new Price().setNetAmount(BigDecimal.TEN)))
            );

        tender = plugin.master(null, tender, null);

        assertEquals(BigDecimal.ONE, tender.getFinalPrice().getNetAmount());
        assertEquals(BigDecimal.ONE, tender.getLots().get(0).getBids().get(0).getPrice().getNetAmount());
    }
}
