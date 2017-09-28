package eu.dl.worker.indicator.plugin;

import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.indicator.TenderIndicatorType;
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
import static org.junit.Assert.assertNull;

/**
 * Test of electronic auction indicator plugin.
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
            .setLots(Arrays.asList(new MasterTenderLot().setValidBidsCount(0)));

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
                    new Publication().setFormType(PublicationFormType.CONTRACT_AWARD)
                            .setPublicationDate(awardDate),
                    new Publication().setFormType(PublicationFormType.CONTRACT_AWARD)
                            .setPublicationDate(awardDate3),
                    new Publication().setFormType(PublicationFormType.CONTRACT_IMPLEMENTATION)
                            .setPublicationDate(LocalDate.MAX)
            ))
            .setLots(Arrays.asList(new MasterTenderLot()
                    .setBids(Arrays.asList(
                            new MasterBid()
                                    .setIsWinning(true)
                                    .setBidders(Arrays.asList(winnerBody1))
                            )
                    ))
            );

    private final MasterTender tender6 = new MasterTender()
            .setPublications(Arrays.asList(
                    new Publication().setFormType(PublicationFormType.CONTRACT_AWARD)
                            .setPublicationDate(awardDate2),
                    new Publication().setFormType(PublicationFormType.CONTRACT_AWARD)
                            .setPublicationDate(LocalDate.MAX)
            ))
            .setLots(Arrays.asList(new MasterTenderLot()
                    .setBids(Arrays.asList(
                            new MasterBid()
                                    .setIsWinning(true)
                                    .setBidders(Arrays.asList(winnerBody2))
                            )
                    ))
            );

    private final MasterTender tender7 = new MasterTender()
            .setPublications(Arrays.asList(
                    new Publication().setFormType(PublicationFormType.CONTRACT_AWARD)
                            .setPublicationDate(awardDate2),
                    new Publication().setFormType(PublicationFormType.CONTRACT_AWARD)
                            .setPublicationDate(LocalDate.MAX)
            ))
            .setLots(Arrays.asList(new MasterTenderLot()
                    .setBids(Arrays.asList(
                            new MasterBid()
                                    .setIsWinning(true)
                                    .setBidders(Arrays.asList(winnerBody3))
                            )
                    ))
            );

    private final MasterTender tender8 = new MasterTender()
            .setPublications(Arrays.asList(
                    new Publication().setFormType(PublicationFormType.CONTRACT_NOTICE)
                            .setPublicationDate(awardDate2),
                    new Publication().setFormType(PublicationFormType.CONTRACT_AMENDMENT)
                            .setPublicationDate(LocalDate.MAX)
            )).setLots(Arrays.asList(new MasterTenderLot()
                    .setBids(Arrays.asList(
                            new MasterBid()
                                    .setIsWinning(true)
                                    .setBidders(Arrays.asList(winnerBody3))
                            )
                    ))
            );

    private final MasterTender tender9 = new MasterTender()
            .setPublications(Arrays.asList(
                    new Publication().setFormType(PublicationFormType.CONTRACT_AWARD)
                            .setPublicationDate(awardDate2),
                    new Publication().setFormType(PublicationFormType.CONTRACT_AWARD)
                            .setPublicationDate(LocalDate.MAX)
            ))
            .setLots(Arrays.asList(new MasterTenderLot()
                    .setBids(Arrays.asList(
                            new MasterBid()
                                    .setIsWinning(true)
                                    .setBidders(Arrays.asList(winnerBody4))
                            )
                    ))
            );

    private final NewCompanyIndicatorPlugin plugin = new NewCompanyIndicatorPlugin();

    /**
     * Test of correct tender address.
     */
    @Test
    public void noIndicatorTest() {
        assertNull(plugin.evaulate(null));
        assertNull(plugin.evaulate(nullTender));
        assertNull(plugin.evaulate(tender1));
        assertNull(plugin.evaulate(tender2));
        assertNull(plugin.evaulate(tender4));
        assertNull(plugin.evaulate(tender6));
        assertNull(plugin.evaulate(tender8));
        assertNull(plugin.evaulate(tender9));
    }

    /**
     * Test of positive result.
     */
    @Test
    public void okTest() {
        assertEquals(plugin.evaulate(tender5).getType(),
                TenderIndicatorType.CORRUPTION_NEW_COMPANY.name());
        assertEquals(plugin.evaulate(tender7).getType(),
                TenderIndicatorType.CORRUPTION_NEW_COMPANY.name());
    }

    /**
     * Test of correct plugin type.
     */
    @Test
    public void getTypeTest() {
        assertEquals(plugin.getType(),
                TenderIndicatorType.CORRUPTION_NEW_COMPANY.name());
    }

    /**
     * Creates winner master body.
     *
     * @param date foundation date
     *
     * @return master body
     *
     */
    private static MasterBody createMasterBody(final String date) {
        MasterBody body = new MasterBody();
        HashMap<String, Object> meta = new HashMap<String, Object>();
        meta.put("foundationDate", date);
        body.setMetaData(meta);
        return body;
    }
}
