package eu.dl.worker.matched;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.junit.Test;

import eu.dl.dataaccess.dao.MatchedBodyDAO;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.generic.Address;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.worker.matched.plugin.ApproximateMatchingPlugin;
import eu.dl.worker.matched.plugin.BaseApproximateMatchingPlugin;
import eu.dl.worker.matched.plugin.MatchingResult;

/**
 * Test class of the ApproximateMatchingPlugin.
 *
 * @author Tomas Mrazek
 */
public class ApproximateMatchingPluginTest {    
    /**
     * Set of body identifiers used for composing MatchedBody instances.
     */
    private final List<BodyIdentifier> bodyIds = Arrays.asList(
            new BodyIdentifier()
                .setId("1")
                .setScope(BodyIdentifier.Scope.CZ)
                .setType(BodyIdentifier.Type.ORGANIZATION_ID),
            new BodyIdentifier()
                .setId("2")
                .setScope(BodyIdentifier.Scope.CZ)
                .setType(BodyIdentifier.Type.VAT),
            new BodyIdentifier()
                .setId("3")
                .setScope(BodyIdentifier.Scope.CZ)
                .setType(BodyIdentifier.Type.HEADER_ICO));
    
    /**
     * Set of MatchedBody instances used for test purposes.
     */
    private final List<MatchedBody> matchedBodies = Arrays.asList(
        new MatchedBody()
            .setStandardizedName("abc")
            .setStandardizedAddress("addr1")
            .setDigest("ab|ad1")
            .setBodyIds(Arrays.asList(bodyIds.get(0)))
            .setGroupId("1")
            .setAddress(new Address()
                .addNuts("CZ041")
                .setPostcode("12345")),
        new MatchedBody()
            .setStandardizedName("efg")
            .setStandardizedAddress("addr1")
            .setDigest("ef|ad1")
            .setBodyIds(Arrays.asList(bodyIds.get(1), bodyIds.get(2)))
            .setGroupId("2")
            .setAddress(new Address()
                .addNuts("CZ045")
                .setPostcode("56789")),
        new MatchedBody()
            .setStandardizedName("abc")
            .setStandardizedAddress("addr12")
            .setDigest("ab|ad1")
            .setBodyIds(Arrays.asList(bodyIds.get(0)))
            .setGroupId("1")
            .setAddress(new Address()
                .addNuts("CZ041")
                .setPostcode("12340")),
        new MatchedBody()
            .setStandardizedName("ijk")
            .setStandardizedAddress("addr3")
            .setDigest("ij|ad3")
            .setBodyIds(Arrays.asList(bodyIds.get(0), bodyIds.get(1), bodyIds.get(2)))
            .setGroupId("3")
            .setAddress(new Address()
                .addNuts("CZ058")
                .setPostcode("89067")));
    
    /**
     * Test of case when the match is found in matchedBody collection.
     */
    @Test
    public final void foundMatchTest() {
        MatchedBody testMatchedBody = matchedBodies.get(0);
        
        //mocking of the MatchedBodyDAO
        MatchedBodyDAO mockedMatchedBodyDAO = mock(MatchedBodyDAO.class);

        when(mockedMatchedBodyDAO.getApproximateMatchBodiesPool(testMatchedBody.getStandardizedName(),
                testMatchedBody.getStandardizedAddress(), testMatchedBody.getBodyIds(), testMatchedBody.getDigest()))
            .thenReturn(matchedBodies);
        
        //plugin test
        BaseApproximateMatchingPlugin plugin = new ApproximateMatchingPlugin(mockedMatchedBodyDAO);
        MatchingResult result = plugin.match(testMatchedBody);
        
        assertEquals(result.getGroupId(), "1");
        assertEquals(result.getMatched(), true);
        assertEquals(result.getMatchedBy(), "approximate");
    }
    
    /**
     * Test of case when the match is found in matchedBody collection but score of this match is lower than limit.
     */
    @Test
    public final void tooLowScoreMatchTest() {
        MatchedBody testMatchedBody = new MatchedBody()
           .setStandardizedName("xyz")
           .setStandardizedAddress("addr1")
           .setDigest("xy|ad1")
           .setBodyIds(null);
        
        //mocking of the MatchedBodyDAO
        MatchedBodyDAO mockedMatchedBodyDAO = mock(MatchedBodyDAO.class);

        when(mockedMatchedBodyDAO.getApproximateMatchBodiesPool(testMatchedBody.getStandardizedName(),
                testMatchedBody.getStandardizedAddress(), testMatchedBody.getBodyIds(), testMatchedBody.getDigest()))
            .thenReturn(Arrays.asList(matchedBodies.get(0), matchedBodies.get(1), matchedBodies.get(2)));
        
        BaseApproximateMatchingPlugin plugin = new ApproximateMatchingPlugin(mockedMatchedBodyDAO);
        MatchingResult result = plugin.match(testMatchedBody);
        
        assertEquals(result.getMatched(), false);
        assertEquals(result.getGroupId(), null);
        assertEquals(result.getMatchedBy(), null);
    }
    
    /**
     * Test of case when the match is not found in matchedBody collection.
     */
    @Test
    public final void notFoundMatchTest() {
        MatchedBody testMatchedBody = new MatchedBody()
           .setStandardizedName("xyz")
           .setStandardizedAddress("addr8")
           .setDigest("xy|ad8")
           .setBodyIds(null);
        
        //mocking of the MatchedBodyDAO
        MatchedBodyDAO mockedMatchedBodyDAO = mock(MatchedBodyDAO.class);

        when(mockedMatchedBodyDAO.getApproximateMatchBodiesPool(testMatchedBody.getStandardizedName(),
                testMatchedBody.getStandardizedAddress(), testMatchedBody.getBodyIds(), testMatchedBody.getDigest()))
            .thenReturn(Collections.emptyList());
        
        BaseApproximateMatchingPlugin plugin = new ApproximateMatchingPlugin(mockedMatchedBodyDAO);
        MatchingResult result = plugin.match(testMatchedBody);
        
        assertEquals(result.getMatched(), false);
        assertEquals(result.getGroupId(), null);
        assertEquals(result.getMatchedBy(), null);
    }

    /**
     * Test of case when more matches exist with max score.
     */
    //@Test
    public final void moreMatchesWithMaxScoreTest() {
        MatchedBody testMatchedBody = matchedBodies.get(0);

        //mocking of the MatchedBodyDAO
        MatchedBodyDAO mockedMatchedBodyDAO = mock(MatchedBodyDAO.class);

        when(mockedMatchedBodyDAO.getApproximateMatchBodiesPool(testMatchedBody.getStandardizedName(),
                testMatchedBody.getStandardizedAddress(), testMatchedBody.getBodyIds(), testMatchedBody.getDigest()))
            .thenReturn(matchedBodies);

        //plugin test
        BaseApproximateMatchingPlugin plugin = new ApproximateMatchingPlugin(mockedMatchedBodyDAO);
        MatchingResult result = plugin.match(testMatchedBody);

        assertEquals(result.getGroupId(), "1");
        assertEquals(result.getMatched(), true);
        assertEquals(result.getMatchedBy(), "approximate");
    }
}
