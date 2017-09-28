package eu.dl.worker.master.plugin;

import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.dataaccess.dto.matched.MatchedTenderLot;
import eu.dl.dataaccess.dto.matched.StructuredLotId;
import org.junit.Test;

import java.time.LocalDate;
import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

/**
 * Test of cancellation fields mastering.
 * The test uses four matched tenders:
 *  - tender1 ... contract notice
 *  - tender2 ... contract award
 *  - tender3 ... contract cancellation
 *  - tender4 ... contract award
 *
 * @author Marek Mikes
 */
public final class CancellationFieldsPluginTest {
    private static final String TENDER1_ID = "tender1ID";
    private static final String TENDER2_ID = "tender2ID";
    private static final String TENDER3_ID = "tender3ID";
    private static final String TENDER4_ID = "tender4ID";

    private LocalDate yesterday = LocalDate.now().minusDays(1);

    private final MatchedTender tender1 = new MatchedTender()
            .addLot(new MatchedTenderLot()
                    .setStructuredId(new StructuredLotId()
                            .setTenderId(TENDER1_ID))
                    .setPublicationDate(LocalDate.now().minusDays(3)))
            .setPublications(Arrays.asList(new Publication()
                    .setIsIncluded(true)
                    .setFormType(PublicationFormType.CONTRACT_NOTICE)
                    .setPublicationDate(LocalDate.now().minusDays(3))));
    private final MatchedTender tender2 = new MatchedTender()
            .addLot(new MatchedTenderLot()
                    .setStructuredId(new StructuredLotId()
                            .setTenderId(TENDER2_ID))
                    .setPublicationDate(LocalDate.now().minusDays(2)))
            .setPublications(Arrays.asList(new Publication()
                    .setIsIncluded(true)
                    .setFormType(PublicationFormType.CONTRACT_AWARD)
                    ));
    private final MatchedTender tender3 = new MatchedTender()
            .addLot(new MatchedTenderLot()
                    .setStructuredId(new StructuredLotId()
                            .setTenderId(TENDER3_ID))
                    .setCancellationDate(yesterday)
                    .setCancellationReason("For fun (text in lot).")
                    .setPublicationDate(yesterday))
            .setCancellationDate(yesterday)
            .setCancellationReason("For fun (text in tender).")
            .setIsWholeTenderCancelled(true)
            .setPublications(Arrays.asList(new Publication()
                    .setIsIncluded(true)
                    .setFormType(PublicationFormType.CONTRACT_CANCELLATION)
                    ));
    private final MatchedTender tender4 = new MatchedTender()
            .addLot(new MatchedTenderLot()
                    .setStructuredId(new StructuredLotId()
                            .setTenderId(TENDER4_ID)
                    )
                    .setPublicationDate(LocalDate.now()))
            .setPublications(Arrays.asList(new Publication()
                    .setIsIncluded(true)
                    .setFormType(PublicationFormType.CONTRACT_AWARD)));

    /**
     * Default constructor to set tender IDs. The setter does not support fluent interface, so we call it here.
     */
    public CancellationFieldsPluginTest() {
        tender1.setId(TENDER1_ID);
        tender2.setId(TENDER2_ID);
        tender3.setId(TENDER3_ID);
        tender4.setId(TENDER4_ID);
    }

    /**
     * Test of situation where contract is cancelled finally.
     */
    @Test
    public void contractCancelledTest() {
        final List<MatchedTender> tenders = Arrays.asList(tender1, tender2, tender3);

        // lots:
        CancellationFieldsPlugin lotPlugin = new CancellationFieldsPlugin(
                Arrays.asList("cancellationDate", "cancellationReason"));
        MasterTenderLot masterTenderLot = new MasterTenderLot();
        lotPlugin.master(Arrays.asList(tender1.getLots().get(0), tender2.getLots().get(0), tender3.getLots().get(0)),
                masterTenderLot, tenders);

        assertTrue(masterTenderLot.getCancellationDate().isEqual(yesterday));
        assertTrue(masterTenderLot.getCancellationReason().equals("For fun (text in lot)."));

        // tenders:
        CancellationFieldsPlugin tenderPlugin = new CancellationFieldsPlugin(
                Arrays.asList("cancellationDate", "cancellationReason", "isWholeTenderCancelled"));
        MasterTender masterTender = new MasterTender();
        tenderPlugin.master(tenders, masterTender, tenders);

        assertTrue(masterTender.getCancellationDate().isEqual(yesterday));
        assertTrue(masterTender.getCancellationReason().equals("For fun (text in tender)."));
    }

    /**
     * Test of situation where contract is cancelled and then it is awarded again.
     */
    @Test
    public void contractCancelledAndThenAwardedTest() {
        final List<MatchedTender> tenders = Arrays.asList(tender1, tender2, tender3, tender4);

        // lots:
        CancellationFieldsPlugin lotPlugin = new CancellationFieldsPlugin(
                Arrays.asList("cancellationDate", "cancellationReason"));
        MasterTenderLot masterTenderLot = new MasterTenderLot();
        lotPlugin.master(Arrays.asList(tender1.getLots().get(0), tender2.getLots().get(0), tender3.getLots().get(0),
                tender4.getLots().get(0)), masterTenderLot, tenders);

        assertNull(masterTenderLot.getCancellationDate());
        assertNull(masterTenderLot.getCancellationReason());

        // tenders:
        CancellationFieldsPlugin tenderPlugin = new CancellationFieldsPlugin(
                Arrays.asList("cancellationDate", "cancellationReason", "isWholeTenderCancelled"));
        MasterTender masterTender = new MasterTender();
        tenderPlugin.master(tenders, masterTender, tenders);

        assertNull(masterTender.getCancellationDate());
        assertNull(masterTender.getCancellationReason());
    }
}
