package eu.dl.worker.master.plugin;

import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.worker.master.plugin.generic.MedianPlugin;
import org.junit.Test;

import java.util.Arrays;

import static org.junit.Assert.assertTrue;

/**
 * Created by michal on 4.1.17.
 */
public class MedianPluginTest {
    private MatchedTender matchedTender1 = new MatchedTender()
            .setEstimatedDurationInDays(1);

    private MatchedTender matchedTender2 = new MatchedTender()
            .setEstimatedDurationInDays(5);

    private MatchedTender matchedTender3 = new MatchedTender()
            .setEstimatedDurationInDays(2);

    private MasterTender masterTender = new MasterTender().setMasterBy("test");

    /**
     * Test Bodies Plugin.
     */
    @Test
    public final void bodiesPluginTest() {
        MasterPlugin masterPlugin = new MedianPlugin("estimatedDurationInDays");

        masterPlugin.master(Arrays.asList(matchedTender1, matchedTender2, matchedTender3),
                masterTender,
                Arrays.asList(matchedTender1, matchedTender2, matchedTender3));
        assertTrue(masterTender.getEstimatedDurationInDays().equals(2));

        masterPlugin.master(Arrays.asList(matchedTender1, matchedTender2, matchedTender3, matchedTender2,
                matchedTender2, matchedTender2, matchedTender2),
                masterTender,
                Arrays.asList(matchedTender1, matchedTender2, matchedTender3, matchedTender2,
                        matchedTender2, matchedTender2, matchedTender2));
        assertTrue(masterTender.getEstimatedDurationInDays().equals(5));

        masterPlugin.master(Arrays.asList(matchedTender1, matchedTender2, matchedTender3, matchedTender2,
                matchedTender2, matchedTender2, matchedTender2, new MatchedTender()),
                masterTender,
                Arrays.asList(matchedTender1, matchedTender2, matchedTender3, matchedTender2,
                        matchedTender2, matchedTender2, matchedTender2, new MatchedTender()));
        assertTrue(masterTender.getEstimatedDurationInDays().equals(5));
    }
}
