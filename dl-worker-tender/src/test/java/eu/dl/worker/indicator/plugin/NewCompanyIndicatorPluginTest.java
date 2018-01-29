package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.indicator.IndicatorStatus;
import eu.dl.dataaccess.dto.master.MasterBid;
import eu.dl.dataaccess.dto.master.MasterBody;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import org.junit.Test;

import java.time.LocalDate;
import java.time.format.DateTimeFormatterBuilder;
import java.time.temporal.ChronoField;
import java.util.Arrays;
import java.util.HashMap;

import static org.junit.Assert.assertEquals;

/**
 * Test of new company indicator plugin.
 *
 * @author Jakub Krafka
 */
public final class NewCompanyIndicatorPluginTest {

    private static final MasterBody winnerBody1 = createMasterBody("2016");

    private static final MasterBody winnerBody2 = createMasterBody("201502");

    private static final MasterBody winnerBody3 = createMasterBody("20160405");

    private static final MasterBody winnerBody4 = createMasterBody("");

    private static final LocalDate awardDate =
            LocalDate.parse("201603", new DateTimeFormatterBuilder()
                    .appendPattern("yyyyMM")
                    .parseDefaulting(ChronoField.DAY_OF_MONTH, 1)
                    .toFormatter());

    private static final LocalDate awardDate2 =
            LocalDate.parse("20170102", new DateTimeFormatterBuilder()
                    .appendPattern("yyyyMMdd")
                    .parseDefaulting(ChronoField.DAY_OF_MONTH, 1)
                    .toFormatter());

    private static final LocalDate awardDate3 =
            LocalDate.parse("20170104", new DateTimeFormatterBuilder()
                    .appendPattern("yyyyMMdd")
                    .parseDefaulting(ChronoField.DAY_OF_MONTH, 1)
                    .toFormatter());

    private final MasterTender nullTender = new MasterTender()
            .setLots(Arrays.asList(new MasterTenderLot()));

    private final MasterTender tender1 = new MasterTender()
            .setLots(Arrays.asList(new MasterTenderLot()
                    .setValidBidsCount(0)));

    private final MasterTender tender2 = new MasterTender()
            .setLots(Arrays.asList(new MasterTenderLot()
                    .setValidBidsCount(2)));

    private final MasterTender tender3 = new MasterTender()
            .setLots(Arrays.asList(new MasterTenderLot()
                    .setBidsCount(0)));

    private final MasterTender tender4 = new MasterTender()
            .setLots(Arrays.asList(new MasterTenderLot()
                    .setBidsCount(1)));

    private final MasterTender tender5 = new MasterTender()
            .setPublications(Arrays.asList(
                    new Publication()
                            .setIsIncluded(true)
                            .setFormType(PublicationFormType.CONTRACT_AWARD)
                            .setPublicationDate(awardDate),
                    new Publication()
                            .setIsIncluded(true)
                            .setFormType(PublicationFormType.CONTRACT_AWARD)
                            .setPublicationDate(awardDate3),
                    new Publication()
                            .setIsIncluded(true)
                            .setFormType(PublicationFormType.CONTRACT_IMPLEMENTATION)
                            .setPublicationDate(LocalDate.MAX)))
            .setLots(Arrays.asList(new MasterTenderLot()
                    .setBids(Arrays.asList(
                            new MasterBid()
                                    .setIsWinning(true)
                                    .setBidders(Arrays.asList(winnerBody1))))));

    private final MasterTender tender6 = new MasterTender()
            .setPublications(Arrays.asList(
                    new Publication()
                            .setIsIncluded(true)
                            .setFormType(PublicationFormType.CONTRACT_AWARD)
                            .setPublicationDate(awardDate2),
                    new Publication()
                            .setIsIncluded(true)
                            .setFormType(PublicationFormType.CONTRACT_AWARD)
                            .setPublicationDate(LocalDate.MAX)))
            .setLots(Arrays.asList(new MasterTenderLot()
                    .setBids(Arrays.asList(
                            new MasterBid()
                                    .setIsWinning(true)
                                    .setBidders(Arrays.asList(winnerBody2))))));

    private final MasterTender tender7 = new MasterTender()
            .setPublications(Arrays.asList(
                    new Publication()
                            .setIsIncluded(true)
                            .setFormType(PublicationFormType.CONTRACT_AWARD)
                            .setPublicationDate(awardDate2),
                    new Publication()
                            .setIsIncluded(true)
                            .setFormType(PublicationFormType.CONTRACT_AWARD)
                            .setPublicationDate(LocalDate.MAX)))
            .setLots(Arrays.asList(new MasterTenderLot()
                    .setBids(Arrays.asList(
                            new MasterBid()
                                    .setIsWinning(true)
                                    .setBidders(Arrays.asList(winnerBody3))))));

    private final MasterTender tender8 = new MasterTender()
            .setPublications(Arrays.asList(
                    new Publication()
                            .setIsIncluded(true)
                            .setFormType(PublicationFormType.CONTRACT_NOTICE)
                            .setPublicationDate(awardDate2),
                    new Publication()
                            .setIsIncluded(true)
                            .setFormType(PublicationFormType.CONTRACT_AMENDMENT)
                            .setPublicationDate(LocalDate.MAX)))
            .setLots(Arrays.asList(new MasterTenderLot()
                    .setBids(Arrays.asList(
                            new MasterBid()
                                    .setIsWinning(true)
                                    .setBidders(Arrays.asList(winnerBody3))))));

    private final MasterTender tender9 = new MasterTender()
            .setPublications(Arrays.asList(
                    new Publication()
                            .setFormType(PublicationFormType.CONTRACT_AWARD)
                            .setPublicationDate(awardDate2),
                    new Publication()
                            .setFormType(PublicationFormType.CONTRACT_AWARD)
                            .setPublicationDate(LocalDate.MAX)))
            .setLots(Arrays.asList(new MasterTenderLot()
                    .setBids(Arrays.asList(
                            new MasterBid()
                                    .setIsWinning(true)
                                    .setBidders(Arrays.asList(winnerBody4))))));

    private final NewCompanyIndicatorPlugin plugin = new NewCompanyIndicatorPlugin();

    /**
     * Test of undefined indicators.
     */
    @Test
    public void undefinedIndicatorTest() {
        assertEquals(plugin.evaluate(nullTender).getStatus(), IndicatorStatus.UNDEFINED);
        assertEquals(plugin.evaluate(tender1).getStatus(), IndicatorStatus.UNDEFINED);
        assertEquals(plugin.evaluate(tender2).getStatus(), IndicatorStatus.UNDEFINED);
        assertEquals(plugin.evaluate(tender3).getStatus(), IndicatorStatus.UNDEFINED);
        assertEquals(plugin.evaluate(tender4).getStatus(), IndicatorStatus.UNDEFINED);
        assertEquals(plugin.evaluate(tender8).getStatus(), IndicatorStatus.UNDEFINED);
        assertEquals(plugin.evaluate(tender9).getStatus(), IndicatorStatus.UNDEFINED);
    }

    /**
     * Test of insufficient indicators.
     */
    @Test
    public void insufficientIndicatorTest() {
        assertEquals(plugin.evaluate(null).getStatus(), IndicatorStatus.INSUFFICIENT_DATA);
    }

    /**
     * Test of calculated indicators.
     */
    @Test
    public void calculatedIndicatorTest() {
        assertEquals(plugin.evaluate(tender5).getStatus(), IndicatorStatus.CALCULATED);
        assertEquals(plugin.evaluate(tender5).getValue(), new Double(0d));

        assertEquals(plugin.evaluate(tender6).getStatus(), IndicatorStatus.CALCULATED);
        assertEquals(plugin.evaluate(tender6).getValue(), new Double(100d));

        assertEquals(plugin.evaluate(tender7).getStatus(), IndicatorStatus.CALCULATED);
        assertEquals(plugin.evaluate(tender7).getValue(), new Double(0d));
    }

    /**
     * Creates winner master body.
     *
     * @param date foundation date
     *
     * @return master body
     */
    private static MasterBody createMasterBody(final String date) {
        MasterBody body = new MasterBody();
        HashMap<String, Object> meta = new HashMap<String, Object>();
        meta.put("foundationDate", date);
        body.setMetaData(meta);
        return body;
    }
}
