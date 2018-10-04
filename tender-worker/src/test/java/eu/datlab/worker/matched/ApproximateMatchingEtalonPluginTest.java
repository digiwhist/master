package eu.datlab.worker.matched;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.List;

import org.junit.Test;

import eu.datlab.dataaccess.dto.matched.BVDEtalonBody;
import eu.dl.dataaccess.dao.EtalonBodyDAO;
import eu.dl.dataaccess.dao.MatchedBodyDAO;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.matched.EtalonBody;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.worker.matched.plugin.ApproximateMatchingEtalonPlugin;
import eu.dl.worker.matched.plugin.BaseApproximateMatchingPlugin;
import eu.dl.worker.matched.plugin.MatchingResult;

/**
 * Test class of the ApproximateMatchingEtalonPlugin.
 *
 * @author Tomas Mrazek
 */
public final class ApproximateMatchingEtalonPluginTest {
    /**
     * Set of body identifiers used for composing MatchedBody instances.
     */
    private final List<BodyIdentifier> bodyIds = Arrays.asList(
            new BodyIdentifier()
                .setId("1")
                .setScope(BodyIdentifier.Scope.CZ)
                .setType(BodyIdentifier.Type.TAX_ID),
            new BodyIdentifier()
                .setId("2")
                .setScope(BodyIdentifier.Scope.EU)
                .setType(BodyIdentifier.Type.TAX_ID),
            new BodyIdentifier()
                .setId("3")
                .setScope(BodyIdentifier.Scope.EU)
                .setType(BodyIdentifier.Type.STATISTICAL));
    
    /**
     * Set of MatchedBody instances used for test purposes.
     */
    private final List<EtalonBody> etalonBodies = Arrays.asList(
        new BVDEtalonBody()
            .setName("abc")
            .setStandardizedName("abc")
            .setStandardizedAddress("addr1")
            .setDigest("ab|ad1")
            .setCountryIsoCode("CZ")
            .setVatTaxNumber(bodyIds.get(0).getId())
            .setId("1"),
        new BVDEtalonBody()
            .setName("efg")
            .setStandardizedName("efg")
            .setStandardizedAddress("addr1")
            .setDigest("ef|ad1")
            .setCountryIsoCode("SK")
            .setEuropeanVatNumber(bodyIds.get(1).getId())
            .setStatisticalNumber(bodyIds.get(2).getId())
            .setId("2"),
        new BVDEtalonBody()
            .setName("abc")
            .setStandardizedName("abc")
            .setStandardizedAddress("addr2")
            .setDigest("ab|ad2")
            .setCountryIsoCode("CZ")
            .setVatTaxNumber(bodyIds.get(0).getId())
            .setId("3"),
        new BVDEtalonBody()
            .setName("ijk")
            .setStandardizedName("ijk")
            .setStandardizedAddress("addr3")
            .setDigest("ij|ad3")
            .setCountryIsoCode("CZ")
            .setVatTaxNumber(bodyIds.get(0).getId())
            .setEuropeanVatNumber(bodyIds.get(1).getId())
            .setStatisticalNumber(bodyIds.get(2).getId())
                    .setId("4"));
    
    private final MatchedBody matchedBody = new MatchedBody()
            .setName("abc")
            .setStandardizedName("abc")
            .setStandardizedAddress("addr1")
            .setDigest("ab|ad1")
            .addBodyId(new BodyIdentifier()
                .setScope(BodyIdentifier.Scope.ETALON_ID)
                .setId("1"))
            .setGroupId("group1");
    
    /**
     * Test of case when the match is found in EtalonBody collection.
     */
    @Test
    public void etalonFoundMatchedFoundTest() {
        MatchedBody testBody = etalonBodies.get(0).getAsMatchedBody();
        
        //mocking of the MatchedBodyDAO and EtalonBody
        MatchedBodyDAO mockedMatchedBodyDAO = mock(MatchedBodyDAO.class);
        EtalonBodyDAO mockedEtalonBodyDAO = mock(EtalonBodyDAO.class);
        
        when(mockedEtalonBodyDAO.getApproximateMatchBodiesPool(testBody.getStandardizedName(),
                testBody.getStandardizedAddress(), testBody.getBodyIds(), testBody.getDigest()))
            .thenReturn(etalonBodies);

        when(mockedMatchedBodyDAO.getByEtalonId(etalonBodies.get(0).getId()))
            .thenReturn(matchedBody);
        
        
        BaseApproximateMatchingPlugin plugin =
            new ApproximateMatchingEtalonPlugin(mockedMatchedBodyDAO, mockedEtalonBodyDAO, "SomeSourceID");
        MatchingResult result = plugin.match(testBody);

        assertEquals("group1", result.getGroupId());
        assertEquals(result.getMatched(), true);
        assertEquals(result.getMatchedBy(), "approximateEtalon");
    }
    
    /**
     * Test of case when the match is found in EtalonBody collection.
     */
    @Test
    public void etalonFoundMatchedNotFoundTest() {
        MatchedBody testBody = etalonBodies.get(0).getAsMatchedBody();
        
        //mocking of the MatchedBodyDAO and EtalonBody
        MatchedBodyDAO mockedMatchedBodyDAO = mock(MatchedBodyDAO.class);
        EtalonBodyDAO mockedEtalonBodyDAO = mock(EtalonBodyDAO.class);

        when(mockedEtalonBodyDAO.getApproximateMatchBodiesPool(testBody.getStandardizedName(),
                testBody.getStandardizedAddress(), testBody.getBodyIds(), testBody.getDigest()))
            .thenReturn(etalonBodies);
        
        when(mockedMatchedBodyDAO.getByEtalonId(etalonBodies.get(0).getId()))
            .thenReturn(null);
        
        
        BaseApproximateMatchingPlugin plugin =
            new ApproximateMatchingEtalonPlugin(mockedMatchedBodyDAO, mockedEtalonBodyDAO, "SomeSourceID");
        MatchingResult result = plugin.match(testBody);
                
        assertNotNull(result.getGroupId());
        assertEquals(result.getMatched(), true);
        assertEquals(result.getMatchedBy(), "approximateEtalon");
    }
}
