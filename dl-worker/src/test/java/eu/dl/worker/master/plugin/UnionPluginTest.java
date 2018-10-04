package eu.dl.worker.master.plugin;

import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.master.MasterBody;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.worker.master.plugin.generic.UnionPlugin;
import eu.dl.worker.master.plugin.generic.converter.TenderConverter;
import org.junit.Test;

import java.time.LocalDate;
import java.util.Arrays;
import java.util.List;
import java.util.function.Supplier;
import java.util.stream.Stream;

import static org.junit.Assert.assertTrue;

/**
 * This plugin connects multiple arrays in one and deletes duplicities.
 */
public class UnionPluginTest {
    private MatchedTender matchedTender1 = new MatchedTender()
            .setPublications(Arrays.asList(
                    new Publication()
                            .setSourceId("a")
                            .setIsIncluded(true),
                    new Publication()
                            .setSourceId("b")
                            .setPublicationDate(LocalDate.MIN)
                            .setVersion(1)))
            .setBidsRecipient(new MatchedBody()
                    .addBodyId(new BodyIdentifier()
                            .setId("30794323")
                            .setType(BodyIdentifier.Type.ORGANIZATION_ID)
                            .setScope(BodyIdentifier.Scope.SK)));

    private MatchedTender matchedTender2 = new MatchedTender()
            .setPublications(Arrays.asList(
                    new Publication()
                            .setSourceId("a")
                            .setPublicationDate(LocalDate.MIN)
                            .setVersion(1),
                    new Publication()
                            .setSourceId("b"),
                    new Publication()
                            .setSourceId("c")
                            .setPublicationDate(LocalDate.MIN)
                            .setVersion(1)))
            .setBidsRecipient(new MatchedBody()
                    .addBodyId(new BodyIdentifier()
                            .setId("30794323")
                            .setType(BodyIdentifier.Type.ORGANIZATION_ID)
                            .setScope(BodyIdentifier.Scope.SK)));

    private MasterTender masterTender = new MasterTender()
            .setBidsRecipient(new MasterBody());

    /**
     * Test whether UnionPlugin is connecting publications correctly and not leaving duplications.
     */
    @Test
    public final void distinctPublicationsTest() {
        MasterPlugin masterPlugin = new UnionPlugin("publications", new TenderConverter());
        masterPlugin.master(Arrays.asList(matchedTender1, matchedTender2),
                masterTender,
                Arrays.asList(matchedTender1, matchedTender2));

        List<Publication> publications = masterTender.getPublications();

        Supplier<Stream<Publication>> publicationsStream = () -> publications.stream();


        assertTrue(publications.size() == 3);
        assertTrue(publicationsStream.get().anyMatch(n -> n.getSourceId().equals("a") && n.getIsIncluded()));
        assertTrue(publicationsStream.get().anyMatch(n -> n.getSourceId().equals("b")));
        assertTrue(publicationsStream.get().anyMatch(n -> n.getSourceId().equals("c")));
    }

    /**
     * Test whether UnionPlugin is connecting body IDs correctly and not leaving duplications.
     */
    @Test
    public final void distinctBodyIdsTest() {
        MasterPlugin masterPlugin = new UnionPlugin("bodyIds", new TenderConverter());
        masterPlugin.master(Arrays.asList(matchedTender1.getBidsRecipient(), matchedTender2.getBidsRecipient()),
                masterTender.getBidsRecipient(),
                Arrays.asList(matchedTender1.getBidsRecipient(), matchedTender2.getBidsRecipient()));

        assertTrue(masterTender.getBidsRecipient().getBodyIds().size() == 1);
        assertTrue(masterTender.getBidsRecipient().getBodyIds().get(0).getId().equals("30794323"));
        assertTrue(masterTender.getBidsRecipient().getBodyIds().get(0).getType().equals(
                BodyIdentifier.Type.ORGANIZATION_ID));
        assertTrue(masterTender.getBidsRecipient().getBodyIds().get(0).getScope().equals(BodyIdentifier.Scope.SK));
    }
}
