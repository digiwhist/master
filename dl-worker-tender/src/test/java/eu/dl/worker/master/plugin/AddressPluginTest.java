package eu.dl.worker.master.plugin;

import eu.dl.dataaccess.dto.generic.Address;
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

import static org.junit.Assert.assertTrue;

/**
 * Test of address mastering.
 *
 * @author Marek Mikes
 */
public final class AddressPluginTest {
    private static final String TENDER1_ID = "tender1ID";
    private static final String TENDER2_ID = "tender2ID";
    private static final String TENDER3_ID = "tender3ID";

    private final MatchedTender tender1 = new MatchedTender()
            .setAddressOfImplementation(new Address()
                    .addNuts("NutsCode1"))
            .setDocumentsLocation(new Address()
                    .addNuts("NutsCode2"))
            .addLot(new MatchedTenderLot()
                    .setAddressOfImplementation(new Address()
                            .setCity("SomeCity"))
                    .setStructuredId(new StructuredLotId()
                            .setTenderId(TENDER1_ID)))
            .setPublications(Arrays.asList(new Publication()
                    .setIsIncluded(true)
                    .setPublicationDate(LocalDate.now().minusDays(2))));
    private final MatchedTender tender2 = new MatchedTender()
            .setAddressOfImplementation(new Address()
                    .setCity("SomeCity"))
            .setDocumentsLocation(new Address()
                    .addNuts("NutsCode2")
                    .setCity("SomeCity"))
            .addLot(new MatchedTenderLot()
                    .setAddressOfImplementation(new Address()
                            .setCity("SomeCity"))
                    .setStructuredId(new StructuredLotId()
                            .setTenderId(TENDER2_ID)))
            .setPublications(Arrays.asList(new Publication()
                    .setIsIncluded(true)
                    .setPublicationDate(LocalDate.now().minusDays(1))));
    private final MatchedTender tender3 = new MatchedTender()
            .setAddressOfImplementation(new Address()
                    .setCity("SomeCity")
                    .setPostcode("SomePostalCode"))
            .setDocumentsLocation(new Address()
                    .addNuts("NutsCode2"))
            .addLot(new MatchedTenderLot()
                    .setAddressOfImplementation(new Address()
                            .setCity("SomeCity"))
                    .setStructuredId(new StructuredLotId()
                            .setTenderId(TENDER3_ID)))
            .setPublications(Arrays.asList(new Publication()
                    .setIsIncluded(true)
                    .setPublicationDate(LocalDate.now())));

    /**
     * Default constructor to set tender IDs. The setter does not support fluent interface, so we call it here.
     */
    public AddressPluginTest() {
        tender1.setId(TENDER1_ID);
        tender2.setId(TENDER2_ID);
        tender3.setId(TENDER3_ID);
    }

    /**
     * Test of correct tender address.
     */
    @Test
    public void tenderAddressTest() {
        List<MatchedTender> allTenders = Arrays.asList(tender1, tender2, tender3);
        AddressPlugin addressPlugin = new AddressPlugin<MatchedTender, MasterTender, MatchedTender>(
                Arrays.asList("addressOfImplementation", "documentsLocation"));
        MasterTender masterTender = new MasterTender();
        addressPlugin.master(allTenders, masterTender, allTenders);

        // wins address which NUTS code as the only one
        assertTrue(masterTender.getAddressOfImplementation().equals(tender1.getAddressOfImplementation()));

        // wins address which NUTS code and one more field filled
        assertTrue(masterTender.getDocumentsLocation().equals(tender2.getDocumentsLocation()));
    }

    /**
     * Test of correct lot address.
     */
    @Test
    public void lotAddressTest() {
        List<MatchedTender> allTenders = Arrays.asList(tender1, tender2, tender3);
        AddressPlugin addressPlugin = new AddressPlugin<MatchedTenderLot, MasterTenderLot, MatchedTenderLot>(
                Arrays.asList("addressOfImplementation"));
        MasterTenderLot masterTenderLot = new MasterTenderLot();
        addressPlugin.master(
                Arrays.asList(tender1.getLots().get(0), tender2.getLots().get(0), tender3.getLots().get(0)),
                masterTenderLot, allTenders);

        // addresses are the same, so wins the latest one
        assertTrue(masterTenderLot.getAddressOfImplementation().equals(
                tender3.getLots().get(0).getAddressOfImplementation()));
    }
}
