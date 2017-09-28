package eu.dl.worker.master.plugin;

import eu.dl.dataaccess.dto.generic.AwardCriterion;
import eu.dl.dataaccess.dto.generic.Price;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.worker.master.plugin.generic.converter.TenderConverter;
import eu.dl.worker.master.plugin.specific.comparator.MoreValuesComparator;
import eu.dl.worker.master.plugin.generic.comparators.NumberComparator;
import eu.dl.worker.master.plugin.generic.comparators.StringComparator;
import eu.dl.worker.master.plugin.generic.LastValuePlugin;
import org.junit.Test;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertTrue;

/**
 * Tests for Last Value Plugin and different comparators.
 *
 * @author Michal Riha
 */
public class LastValueComparatorsTest {
    // String comparator
    private final String stringValueName = "appealBodyName";
    private final String shortString = "a";
    private final String longString = "aaaaaa";

    private final String numberValueName = "estimatedDurationInYears";
    private final int smallNumber = 1;
    private final int bigNumber = 10;

    private final String awardCriteriaValueName = "awardCriteria";
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

    private final String priceValueName = "netAmount";
    private final BigDecimal smallPrice = BigDecimal.valueOf(1);
    private final BigDecimal bigPrice = BigDecimal.valueOf(10);

    private final MatchedTender matchedTender1 = new MatchedTender()
            .setAppealBodyName(shortString)
            .setEstimatedDurationInYears(bigNumber)
            .setFinalPrice(new Price().setNetAmount(smallPrice))
            .setAwardCriteria(awardCriteria1);

    private final MatchedTender matchedTender2 = new MatchedTender()
            .setAppealBodyName(longString)
            .setEstimatedDurationInYears(smallNumber)
            .setFinalPrice(new Price().setNetAmount(bigPrice))
            .setAwardCriteria(awardCriteria2);

    private final MatchedTender matchedTender3 = new MatchedTender()
            .setAppealBodyName(longString)
            .setEstimatedDurationInYears(smallNumber)
            .setFinalPrice(new Price().setNetAmount(bigPrice))
            .setAwardCriteria(awardCriteria3)
            .setDeposits(shortString)
            .setHash(shortString)
            .setEconomicRequirements(shortString);

    private MasterTender masterTender = new MasterTender()
            .setFinalPrice(new Price());

    // String comparator

    /**
     * Test StringComparator can sort strings ascending by length.
     */
    @Test
    public final void longerStringTest() {
        MasterPlugin masterPlugin = new LastValuePlugin<>(Arrays.asList(stringValueName),
                new StringComparator<>(stringValueName), new TenderConverter());
        masterPlugin.master(Arrays.asList(matchedTender1, matchedTender2), masterTender,
                Arrays.asList(matchedTender1, matchedTender2));

        assertTrue(masterTender.getAppealBodyName().equals(longString));
    }

    /**
     * Test StringComparator can sort strings descending by length.
     */
    @Test
    public final void shorterStringTest() {
        MasterPlugin masterPlugin = new LastValuePlugin(Arrays.asList(stringValueName),
                new StringComparator<>(stringValueName).reversed(), new TenderConverter());
        masterPlugin.master(Arrays.asList(matchedTender1, matchedTender2), masterTender,
                Arrays.asList(matchedTender1, matchedTender2));

        assertTrue(masterTender.getAppealBodyName().equals(shortString));
    }

    // Number comparator

    /**
     * Test NumberComparator can sort numbers ascending.
     */
    @Test
    public final void biggerNumberTest() {
        MasterPlugin masterPlugin = new LastValuePlugin<>(Arrays.asList(numberValueName),
                new NumberComparator<>(numberValueName), new TenderConverter());
        masterPlugin.master(Arrays.asList(matchedTender1, matchedTender2), masterTender,
                Arrays.asList(matchedTender1, matchedTender2));

        assertTrue(masterTender.getEstimatedDurationInYears().equals(bigNumber));
    }

    /**
     * Test NumberComparator can sort numbers ascending.
     */
    @Test
    public final void smallerNumberTest() {
        MasterPlugin masterPlugin = new LastValuePlugin(Arrays.asList(numberValueName),
                new NumberComparator<>(numberValueName).reversed(), new TenderConverter());
        masterPlugin.master(Arrays.asList(matchedTender1, matchedTender2), masterTender,
                Arrays.asList(matchedTender1, matchedTender2));

        assertTrue(masterTender.getEstimatedDurationInYears().equals(smallNumber));
    }

    /**
     * Test NumberComparator can sort plunged numbers.
     */
    @Test
    public final void plungedNumberTest() {
        MasterPlugin masterPlugin = new LastValuePlugin(Arrays.asList(priceValueName),
                new NumberComparator<>(priceValueName).reversed(), new TenderConverter());
        masterPlugin.master(Arrays.asList(matchedTender1.getFinalPrice(), matchedTender2.getFinalPrice()),
                masterTender.getFinalPrice(),
                Arrays.asList(matchedTender1.getFinalPrice(), matchedTender2.getFinalPrice()));

        assertTrue(masterTender.getFinalPrice().getNetAmount().equals(smallPrice));
    }

    // More values comparator

    /**
     * Test MoreValuesComparator can sort objects by number of filled variables it has.
     */
    @Test
    public final void moreValuesTest() {
        MasterPlugin masterPlugin = new LastValuePlugin<>(
                stringValueName, new MoreValuesComparator(), new TenderConverter());

        masterPlugin.master(Arrays.asList(matchedTender1, matchedTender3), masterTender,
                Arrays.asList(matchedTender1, matchedTender3));
        assertTrue(masterTender.getAppealBodyName().equals(longString));

        masterPlugin.master(Arrays.asList(matchedTender3, matchedTender1), masterTender,
                Arrays.asList(matchedTender3, matchedTender1));
        assertTrue(masterTender.getAppealBodyName().equals(longString));
    }
}
