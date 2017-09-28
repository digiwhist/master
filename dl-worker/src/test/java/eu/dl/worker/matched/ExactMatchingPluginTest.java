package eu.dl.worker.matched;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.junit.Test;

import eu.dl.dataaccess.dao.MatchedBodyDAO;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.matched.MatchedGroupInfo;
import eu.dl.worker.matched.plugin.BaseExactMatchingPlugin;
import eu.dl.worker.matched.plugin.ExactMatchingPlugin;
import eu.dl.worker.matched.plugin.MatchingResult;

/**
 * Test class of the ExactMatchingPlugin.
 *
 * @author Tomas Mrazek
 */
public final class ExactMatchingPluginTest {
    /**
     * Set of body identifiers used for composing MatchedBody instances.
     */
    private final List<BodyIdentifier> bodyIds = Arrays.asList(
        new BodyIdentifier().setId("1").setScope(BodyIdentifier.Scope.CZ).setType(BodyIdentifier.Type.ORGANIZATION_ID),
        new BodyIdentifier().setId("2").setScope(BodyIdentifier.Scope.CZ).setType(BodyIdentifier.Type.VAT),
        new BodyIdentifier().setId("3").setScope(BodyIdentifier.Scope.CZ).setType(BodyIdentifier.Type.HEADER_ICO));

    private final MatchedBody testBody = new MatchedBody().setStandardizedName("abc").setStandardizedAddress("addr1")
        .setBodyIds(Arrays.asList(bodyIds.get(0), bodyIds.get(1))).setGroupId("1");

    /**
     * Set of MatchedBody instances used for test purposes, one max similarity with testBody.
     */
    private final List<MatchedBody> poolOneMax = Arrays.asList(
        new MatchedBody().setStandardizedName("abc").setStandardizedAddress("addr1")
            .setBodyIds(Arrays.asList(bodyIds.get(0))).setGroupId("1"),
        new MatchedBody().setStandardizedName("efg").setStandardizedAddress("addr1")
            .setBodyIds(Arrays.asList(bodyIds.get(1), bodyIds.get(2))).setGroupId("2"),
        new MatchedBody().setStandardizedName("abc").setStandardizedAddress("addr2")
            .setBodyIds(Arrays.asList(bodyIds.get(0))).setGroupId("1"),
        new MatchedBody().setStandardizedName("ijk").setStandardizedAddress("addr3")
            .setBodyIds(Arrays.asList(bodyIds.get(0), bodyIds.get(1), bodyIds.get(2))).setGroupId("3"));

    /**
     * Set of MatchedBody instances used for test purposes, many max similarities with testBody.
     */
    private final List<MatchedBody> poolManyMaxes = new ArrayList<>(poolOneMax);
    {
        // appends other maxes
        // second maximum for testBody from group 1
        poolManyMaxes.add(
            new MatchedBody().setStandardizedName("abc").setStandardizedAddress("addr2")
                .setBodyIds(Arrays.asList(bodyIds.get(0), bodyIds.get(1))).setGroupId("1"));
        // first maximum for testBody from group 2
        poolManyMaxes.add(new MatchedBody().setStandardizedName("abc").setStandardizedAddress("addr1")
            .setBodyIds(Arrays.asList(bodyIds.get(0))).setGroupId("2"));
    }


    /**
     * Test of case when the match is found in matchedBody collection.
     */
    @Test
    public void foundOneMaxMatchTest() {
        MatchedBody testMatchedBody = poolOneMax.get(0);
        
        //mocking of the MatchedBodyDAO
        MatchedBodyDAO mockedMatchedBodyDAO = mock(MatchedBodyDAO.class);

        when(mockedMatchedBodyDAO.getExactMatchBodiesPool(testMatchedBody.getStandardizedName(),
                testMatchedBody.getStandardizedAddress(), testMatchedBody.getBodyIds()))
            .thenReturn(poolOneMax);

        //plugin test
        BaseExactMatchingPlugin plugin = new ExactMatchingPlugin(mockedMatchedBodyDAO);
        MatchingResult result = plugin.match(testMatchedBody);
        
        assertEquals(result.getGroupId(), "1");
        assertEquals(result.getMatched(), true);
        assertEquals(result.getMatchedBy(), "exact");
    }
    
    /**
     * Test of case when the match is found in matchedBody collection but score of this match is lower than limit.
     */
    @Test
    public void tooLowScoreMatchTest() {
        MatchedBody testMatchedBody = new MatchedBody()
           .setStandardizedName("xyz")
           .setStandardizedAddress("addr1")
           .setBodyIds(null);
        
        //mocking of the MatchedBodyDAO
        MatchedBodyDAO mockedMatchedBodyDAO = mock(MatchedBodyDAO.class);

        when(mockedMatchedBodyDAO.getExactMatchBodiesPool(testMatchedBody.getStandardizedName(),
                testMatchedBody.getStandardizedAddress(), testMatchedBody.getBodyIds()))
            .thenReturn(Arrays.asList(poolOneMax.get(0), poolOneMax.get(1)));
        
        //plugin test
        BaseExactMatchingPlugin plugin = new ExactMatchingPlugin(mockedMatchedBodyDAO);
        MatchingResult result = plugin.match(testMatchedBody);         
        
        assertEquals(result.getMatched(), false);
        assertEquals(result.getGroupId(), null);
        assertEquals(result.getMatchedBy(), null);
    }
    
    /**
     * Test of case when the match is not found in matchedBody collection.
     */
    @Test
    public void notFoundMatchTest() {
        MatchedBody testMatchedBody = new MatchedBody()
           .setStandardizedName("xyz")
           .setStandardizedAddress("addr8")
           .setBodyIds(null);
        
        //mocking of the MatchedBodyDAO
        MatchedBodyDAO mockedMatchedBodyDAO = mock(MatchedBodyDAO.class);

        when(mockedMatchedBodyDAO.getExactMatchBodiesPool(testMatchedBody.getStandardizedName(),
                testMatchedBody.getStandardizedAddress(), testMatchedBody.getBodyIds()))
            .thenReturn(Collections.emptyList());
        
        //plugin test
        BaseExactMatchingPlugin plugin = new ExactMatchingPlugin(mockedMatchedBodyDAO);
        MatchingResult result = plugin.match(testMatchedBody);         
        
        assertEquals(result.getMatched(), false);
        assertEquals(result.getGroupId(), null);
        assertEquals(result.getMatchedBy(), null);
    }

    /**
     * Examples from practice.
     */
    @Test
    public void practiceTest() {
        // CZ_b8e2875ad16785df345d6243ae2a035a6e40a35ce388de1a53124dc33c997f91_1
        MatchedBody test = new MatchedBody()
            .addBodyId(new BodyIdentifier()
                .setId("00294136").setType(BodyIdentifier.Type.HEADER_ICO).setScope(BodyIdentifier.Scope.CZ))
            .setStandardizedName("autocont cz as")
            .setStandardizedAddress("hornopolni 3322/34 ostrava - moravska ostrava cz");

        // CZ_588bd381781ab59a8c4b869ff9cd475a3b44e34a441b9ed7a82dd996f3ea25cb_1
        MatchedBody pool = new MatchedBody()
            .addBodyId(new BodyIdentifier()
                .setId("00294136").setType(BodyIdentifier.Type.ORGANIZATION_ID).setScope(BodyIdentifier.Scope.CZ))
            .addBodyId(new BodyIdentifier()
                .setId("00294136").setType(BodyIdentifier.Type.HEADER_ICO).setScope(BodyIdentifier.Scope.CZ))
            .setStandardizedName("mesto bystrice nad pernstejnem")
            .setStandardizedAddress("pricni 405 bystrice nad pernstejnem cz");


        //mocking of the MatchedBodyDAO
        MatchedBodyDAO mockedMatchedBodyDAO = mock(MatchedBodyDAO.class);

        when(mockedMatchedBodyDAO.getExactMatchBodiesPool(test.getStandardizedName(),
                test.getStandardizedAddress(), test.getBodyIds()))
            .thenReturn(Arrays.asList(pool));

        //plugin test
        BaseExactMatchingPlugin plugin = new ExactMatchingPlugin(mockedMatchedBodyDAO);
        MatchingResult result = plugin.match(test);

        assertEquals(result.getMatched(), false);
        assertEquals(result.getGroupId(), null);
        assertEquals(result.getMatchedBy(), null);
    }

    /**
     * 
     */
    @Test
    public void foundManyMaxMachesTest() {
        //mocking of the MatchedBodyDAO
        MatchedBodyDAO mockedMatchedBodyDAO = mock(MatchedBodyDAO.class);

        when(mockedMatchedBodyDAO.getExactMatchBodiesPool(testBody.getStandardizedName(),
                testBody.getStandardizedAddress(), testBody.getBodyIds()))
            .thenReturn(poolManyMaxes);

        // group 1 is bigger therefore wins
        when(mockedMatchedBodyDAO.getGroupsInfo(Arrays.asList("1", "2")))
            .thenReturn(Arrays.asList(
                new MatchedGroupInfo().setGroupId("1").setSize(10),
                new MatchedGroupInfo().setGroupId("2").setSize(5)));

        BaseExactMatchingPlugin plugin = new ExactMatchingPlugin(mockedMatchedBodyDAO);
        MatchingResult result = plugin.match(testBody);

        assertEquals(result.getGroupId(), "1");
        assertEquals(result.getMatched(), true);
        assertEquals(result.getMatchedBy(), "exact");


        // group 1 is bigger but group 2 includes etalon, therefore group 2 wins
        when(mockedMatchedBodyDAO.getGroupsInfo(Arrays.asList("1", "2")))
            .thenReturn(Arrays.asList(
                new MatchedGroupInfo().setGroupId("1").setSize(10),
                new MatchedGroupInfo().setGroupId("2").setHasEtalon(true).setSize(5)));

        plugin = new ExactMatchingPlugin(mockedMatchedBodyDAO);
        result = plugin.match(testBody);

        assertEquals(result.getGroupId(), "2");
        assertEquals(result.getMatched(), true);
        assertEquals(result.getMatchedBy(), "exact");


        // both group includes etalon but group 1 is bigger, therefore group 1 wins
        when(mockedMatchedBodyDAO.getGroupsInfo(Arrays.asList("1", "2")))
            .thenReturn(Arrays.asList(
                new MatchedGroupInfo().setGroupId("1").setHasEtalon(true).setSize(10),
                new MatchedGroupInfo().setGroupId("2").setHasEtalon(true).setSize(5)));

        plugin = new ExactMatchingPlugin(mockedMatchedBodyDAO);
        result = plugin.match(testBody);

        assertEquals(result.getGroupId(), "1");
        assertEquals(result.getMatched(), true);
        assertEquals(result.getMatchedBy(), "exact");
    }
}
