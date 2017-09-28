package eu.dl.worker.master.plugin;

import eu.dl.dataaccess.dto.generic.CPV;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.dataaccess.dto.matched.MatchedTenderLot;
import eu.dl.dataaccess.dto.matched.StructuredLotId;
import org.junit.Test;

import java.time.LocalDate;
import java.util.Arrays;

import static org.junit.Assert.assertTrue;

/**
 * Test of CPVs mastering.
 *
 * @author Marek Mikes
 */
public final class CpvPluginTest {
    private static final String TENDER1_ID = "tender1ID";
    private static final String TENDER2_ID = "tender2ID";
    private static final String TENDER3_ID = "tender3ID";
    private static final String TENDER4_ID = "tender4ID";

    private final MatchedTender tender1 = new MatchedTender()
            .addLot(new MatchedTenderLot()
                    .setCpvs(Arrays.asList(
                            new CPV()
                                    .setIsMain(false)
                                    .setCode("1"),
                            new CPV()
                                    .setIsMain(true)
                                    .setCode("2"),
                            new CPV()
                                    .setIsMain(false)
                                    .setCode("3")))
                    .setStructuredId(new StructuredLotId()
                            .setTenderId(TENDER1_ID))
                    .setPublicationDate(LocalDate.now().minusDays(3)));
    private final MatchedTender tender2 = new MatchedTender()
            .addLot(new MatchedTenderLot()
                    .setCpvs(Arrays.asList(
                            new CPV()
                                    .setIsMain(false)
                                    .setCode("1"),
                            new CPV()
                                    .setIsMain(false)
                                    .setCode("4"),
                            new CPV()
                                    .setIsMain(false)
                                    .setCode("5")))
                    .setStructuredId(new StructuredLotId()
                            .setTenderId(TENDER2_ID))
                            .setPublicationDate(LocalDate.now().minusDays(2)));
    private final MatchedTender tender3 = new MatchedTender()
            .addLot(new MatchedTenderLot()
                    .setCpvs(Arrays.asList(
                            new CPV()
                                    .setIsMain(true)
                                    .setCode("6"),
                            new CPV()
                                    .setIsMain(false)
                                    .setCode("1")))
                    .setStructuredId(new StructuredLotId()
                            .setTenderId(TENDER3_ID))
                    .setPublicationDate(LocalDate.now().minusDays(1)));
    private final MatchedTender tender4 = new MatchedTender()
            .addLot(new MatchedTenderLot()
                    .setCpvs(Arrays.asList(
                            new CPV()
                                    .setIsMain(false)
                                    .setCode("6")))
                    .setStructuredId(new StructuredLotId()
                            .setTenderId(TENDER4_ID))
                    .setPublicationDate(LocalDate.now()));

    /**
     * Default constructor to set tender IDs. The setter does not support fluent interface, so we call it here.
     */
    public CpvPluginTest() {
        tender1.setId(TENDER1_ID);
        tender2.setId(TENDER2_ID);
        tender3.setId(TENDER3_ID);
        tender4.setId(TENDER4_ID);
    }

    /**
     * Test of simple union - empty set intersection.
     */
    @Test
    public void simpleUnionTest() {
        CpvPlugin cpvPlugin = new CpvPlugin();
        MasterTenderLot masterTenderLot = new MasterTenderLot();
        cpvPlugin.master(Arrays.asList(tender1.getLots().get(0), tender2.getLots().get(0)), masterTenderLot,
                Arrays.asList(tender1, tender2));

        // 2 is main CPV, then 1,3,4,5
        assertTrue(masterTenderLot.getCpvs().size() == 5);
        for (CPV cpv : masterTenderLot.getCpvs()) {
            if (cpv.getCode().equals("2")) {
                assertTrue(cpv.getIsMain());
            } else {
                assertTrue(!cpv.getIsMain());
            }
        }
    }

    /**
     * Test of complex union - non-empty set intersection and union of two main CPVs.
     */
    @Test
    public void complexUnionTest() {
        CpvPlugin cpvPlugin = new CpvPlugin();
        MasterTenderLot masterTenderLot = new MasterTenderLot();
        cpvPlugin.master(Arrays.asList(tender1.getLots().get(0), tender2.getLots().get(0), tender3.getLots().get(0),
                tender4.getLots().get(0)), masterTenderLot, Arrays.asList(tender1, tender2, tender3, tender4));

        // 6 is main CPV, then 1,3,4,5,6
        assertTrue(masterTenderLot.getCpvs().size() == 6);
        for (CPV cpv : masterTenderLot.getCpvs()) {
            if (cpv.getCode().equals("6")) {
                assertTrue(cpv.getIsMain());
            } else {
                assertTrue(!cpv.getIsMain());
            }
        }
    }
}
