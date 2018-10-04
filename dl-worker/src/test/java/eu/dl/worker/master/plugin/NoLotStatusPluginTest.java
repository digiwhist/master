package eu.dl.worker.master.plugin;

import eu.dl.dataaccess.dto.codetables.TenderLotStatus;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import eu.dl.worker.master.plugin.specific.NoLotStatusPlugin;
import org.junit.Test;

import java.time.LocalDate;
import java.util.Arrays;
import java.util.List;

import static eu.dl.dataaccess.dto.codetables.PublicationFormType.CONTRACT_NOTICE;
import static eu.dl.dataaccess.dto.codetables.PublicationFormType.CONTRACT_AWARD;
import static eu.dl.dataaccess.dto.codetables.PublicationFormType.PRIOR_INFORMATION_NOTICE;
import static eu.dl.dataaccess.dto.codetables.PublicationFormType.CONTRACT_CANCELLATION;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

/**
 * Created by michal on 4.1.17.
 */
public class NoLotStatusPluginTest {

    private static final LocalDate NOW = LocalDate.now();

    /**
     * Test of {@link eu.dl.worker.master.plugin.specific.NoLotStatusPlugin#master(List, MasterTender, List)}.
     */
    @Test
    public final void masterTest() {
        NoLotStatusPlugin plugin = new NoLotStatusPlugin();

        // has lots, plugin is not applied
        MasterTender masterTender = new MasterTender().addLot(new MasterTenderLot());
        masterTender = plugin.master(null, masterTender, null);
        assertNull(masterTender.getLots().get(0).getStatus());

        // no status
        masterTender = new MasterTender();
        masterTender = plugin.master(null, masterTender, null);
        assertNull(masterTender.getLots());


        // publications without publication date (depends on order of publications)
        masterTender = new MasterTender()
            .setPublications(Arrays.asList(
                new Publication().setIsIncluded(true).setFormType(CONTRACT_AWARD),
                new Publication().setIsIncluded(true).setFormType(CONTRACT_CANCELLATION),
                new Publication().setIsIncluded(true).setFormType(CONTRACT_NOTICE)
            ));
        masterTender = plugin.master(null, masterTender, null);
        assertEquals(1, masterTender.getLots().size());
        assertEquals(TenderLotStatus.AWARDED, masterTender.getLots().get(0).getStatus());

        masterTender = new MasterTender()
            .setPublications(Arrays.asList(
                new Publication().setIsIncluded(true).setFormType(CONTRACT_CANCELLATION),
                new Publication().setIsIncluded(true).setFormType(CONTRACT_AWARD),
                new Publication().setIsIncluded(true).setFormType(CONTRACT_NOTICE)
            ));
        masterTender = plugin.master(null, masterTender, null);
        assertEquals(1, masterTender.getLots().size());
        assertEquals(TenderLotStatus.CANCELLED, masterTender.getLots().get(0).getStatus());


        // cancelled
        masterTender = new MasterTender().setIsWholeTenderCancelled(true);
        masterTender = plugin.master(null, masterTender, null);
        assertEquals(1, masterTender.getLots().size());
        assertEquals(TenderLotStatus.CANCELLED, masterTender.getLots().get(0).getStatus());

        masterTender = new MasterTender()
            .setPublications(Arrays.asList(
                new Publication().setIsIncluded(true).setFormType(CONTRACT_AWARD).setPublicationDate(NOW.minusDays(1)),
                new Publication().setIsIncluded(true).setFormType(CONTRACT_CANCELLATION).setPublicationDate(NOW),
                new Publication().setIsIncluded(true).setFormType(CONTRACT_NOTICE).setPublicationDate(NOW.plusDays(1))
            ));
        masterTender = plugin.master(null, masterTender, null);
        assertEquals(1, masterTender.getLots().size());
        assertEquals(TenderLotStatus.CANCELLED, masterTender.getLots().get(0).getStatus());


        // awarded
        masterTender = new MasterTender()
            .setPublications(Arrays.asList(
                new Publication().setIsIncluded(true).setFormType(CONTRACT_AWARD).setPublicationDate(NOW),
                new Publication().setIsIncluded(true).setFormType(CONTRACT_CANCELLATION).setPublicationDate(NOW.minusDays(1)),
                new Publication().setIsIncluded(true).setFormType(PRIOR_INFORMATION_NOTICE).setPublicationDate(NOW.plusDays(1))
            ));
        masterTender = plugin.master(null, masterTender, null);
        assertEquals(1, masterTender.getLots().size());
        assertEquals(TenderLotStatus.AWARDED, masterTender.getLots().get(0).getStatus());

        masterTender = new MasterTender()
            .setPublications(Arrays.asList(
                new Publication().setIsIncluded(true).setFormType(CONTRACT_NOTICE).setPublicationDate(NOW),
                new Publication().setIsIncluded(true).setFormType(PRIOR_INFORMATION_NOTICE).setPublicationDate(NOW.minusDays(1)),
                new Publication().setIsIncluded(true).setFormType(CONTRACT_AWARD).setPublicationDate(NOW.plusDays(1))
            ));
        masterTender = plugin.master(null, masterTender, null);
        assertEquals(1, masterTender.getLots().size());
        assertEquals(TenderLotStatus.AWARDED, masterTender.getLots().get(0).getStatus());


        // announced
        masterTender = new MasterTender()
            .setPublications(Arrays.asList(
                new Publication().setIsIncluded(true).setFormType(CONTRACT_NOTICE).setPublicationDate(NOW),
                new Publication().setIsIncluded(true).setFormType(PRIOR_INFORMATION_NOTICE).setPublicationDate(NOW.minusDays(1))));
        masterTender = plugin.master(null, masterTender, null);
        assertEquals(1, masterTender.getLots().size());
        assertEquals(TenderLotStatus.ANNOUNCED, masterTender.getLots().get(0).getStatus());


        // prepared
        masterTender = new MasterTender()
            .setPublications(Arrays.asList(
                new Publication().setIsIncluded(true).setFormType(CONTRACT_NOTICE).setPublicationDate(NOW.minusDays(1)),
                new Publication().setIsIncluded(true).setFormType(PRIOR_INFORMATION_NOTICE).setPublicationDate(NOW)));
        masterTender = plugin.master(null, masterTender, null);
        assertEquals(1, masterTender.getLots().size());
        assertEquals(TenderLotStatus.PREPARED, masterTender.getLots().get(0).getStatus());
    }
}
