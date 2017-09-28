package eu.dl.worker.master.plugin;

import eu.dl.dataaccess.dto.generic.AwardCriterion;
import eu.dl.dataaccess.dto.generic.Price;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.worker.master.plugin.specific.AwardCriteriaPlugin;
import org.junit.Test;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertTrue;

/**
 * Created by michal on 4.1.17.
 */
public class AwardCriteriaPluginTest {

    private final List<AwardCriterion> awardCriteria1 = Arrays.asList(
            new AwardCriterion().setName("a").setWeight(30),
            new AwardCriterion().setName("b").setWeight(30),
            new AwardCriterion().setName("c").setWeight(40));
    private final List<AwardCriterion> awardCriteria2 = Arrays.asList(
            new AwardCriterion().setName("d").setWeight(30),
            new AwardCriterion().setName("e").setWeight(30));
    private final List<AwardCriterion> awardCriteria3 = Arrays.asList(
            new AwardCriterion().setName("f").setWeight(30),
            new AwardCriterion().setName("g").setWeight(30),
            new AwardCriterion().setName("h").setWeight(40));

    private final BigDecimal smallPrice = BigDecimal.valueOf(1);
    private final BigDecimal bigPrice = BigDecimal.valueOf(10);

    private final MatchedTender matchedTender1 = new MatchedTender()
            .setFinalPrice(new Price().setNetAmount(smallPrice))
            .setAwardCriteria(awardCriteria1)
            .setPublicationDate(LocalDate.MIN);

    private final MatchedTender matchedTender2 = new MatchedTender()
            .setFinalPrice(new Price().setNetAmount(bigPrice))
            .setAwardCriteria(awardCriteria2)
            .setPublicationDate(LocalDate.MAX);

    private final MatchedTender matchedTender3 = new MatchedTender()
            .setFinalPrice(new Price().setNetAmount(bigPrice))
            .setAwardCriteria(awardCriteria3)
            .setPublicationDate(LocalDate.MAX);

    private MasterTender masterTender = new MasterTender()
            .setFinalPrice(new Price());

    /**
     * Test AwardCriteriaComparator is sorting awards correctly.
     */
    @Test
    public final void awardCriteriaModifiedTest() {
        matchedTender1.setPublications(Arrays.asList(new Publication()
                .setIsIncluded(true)))
                .setId("a");
        matchedTender2.setPublications(Arrays.asList(new Publication()
                .setIsIncluded(true)))
                .setId("b");
        matchedTender3.setPublications(Arrays.asList(new Publication()
                .setIsIncluded(true)))
                .setId("c");

        MasterPlugin masterPlugin = new AwardCriteriaPlugin();

        masterPlugin.master(Arrays.asList(matchedTender1, matchedTender3),
                masterTender,
                Arrays.asList(matchedTender1, matchedTender3));
        assertTrue(masterTender.getAwardCriteria().get(0).getName().equals("f"));

        masterPlugin.master(Arrays.asList(matchedTender3, matchedTender1),
                masterTender,
                Arrays.asList(matchedTender3, matchedTender1));
        assertTrue(masterTender.getAwardCriteria().get(0).getName().equals("f"));

        masterPlugin.master(Arrays.asList(matchedTender1, matchedTender2),
                masterTender,
                Arrays.asList(matchedTender1, matchedTender2));
        assertTrue(masterTender.getAwardCriteria().get(0).getName().equals("a"));

        masterPlugin.master(Arrays.asList(matchedTender2, matchedTender1),
                masterTender,
                Arrays.asList(matchedTender2, matchedTender1));
        assertTrue(masterTender.getAwardCriteria().get(0).getName().equals("a"));
    }
}
