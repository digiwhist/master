package eu.dl.worker.master.plugin;

import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.worker.master.plugin.body.BodiesPlugin;
import org.junit.Test;

import java.util.Arrays;

import static org.junit.Assert.assertTrue;

/**
 * Created by michal on 25.11.16.
 */
public class BodiesPluginTest {
    private MatchedTender matchedTender1 = new MatchedTender()
            .addBuyer(new MatchedBody().setName("a").setGroupId("a"))
            .addBuyer(new MatchedBody().setName("b").setGroupId("b"));

    private MatchedTender matchedTender2 = new MatchedTender()
            .addBuyer(new MatchedBody().setName("c").setGroupId("c").setCompletenessScore(3.0));

    private MatchedTender matchedTender3 = new MatchedTender()
            .addBuyer(new MatchedBody().setName("e").setGroupId("e").setCompletenessScore(1.0));

    private MasterTender masterTender = new MasterTender().setMasterBy("test");

    /**
     * Test Bodies Plugin.
     */
    @Test
    public final void bodiesPluginTest() {
        MasterPlugin masterPlugin = new BodiesPlugin("buyers");

        masterPlugin.master(Arrays.asList(matchedTender2, matchedTender3),
                masterTender,
                Arrays.asList(matchedTender2, matchedTender3));
        assertTrue(masterTender.getBuyers().get(0).getName().equals("c"));

        masterPlugin.master(Arrays.asList(matchedTender1, matchedTender3),
                masterTender,
                Arrays.asList(matchedTender1, matchedTender3));
        assertTrue(masterTender.getBuyers().get(0).getName().equals("a"));
        assertTrue(masterTender.getBuyers().get(2).getName().equals("e"));
    }
}
