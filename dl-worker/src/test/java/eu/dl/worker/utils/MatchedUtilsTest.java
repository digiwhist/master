package eu.dl.worker.utils;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.List;
import java.util.function.BiFunction;

import org.apache.lucene.search.spell.LevensteinDistance;
import org.apache.lucene.search.spell.NGramDistance;
import org.junit.Test;

import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.worker.utils.matched.MatchedUtils;

/**
 * MatchedUtils test class.
 *
 * @author Tomas Mrazek
 */
public final class MatchedUtilsTest {
    /**
     * Levenstein matching algorithm.
     */
    private final BiFunction<String, String, Float> levenstein = (s, t) -> {
        final LevensteinDistance ld = new LevensteinDistance();
        return ld.getDistance(s, t);
    };
    
    /**
     * Tests for MatchedUtils#getSimilarity(java.lang.Object, java.lang.Object, java.util.function.BiFunction) function.
     */
    @Test
    public void getSimilarityTest() {
        assertEquals(1.0f, MatchedUtils.getSimilarity("abcd", "abcd", levenstein), 0.0f);        
        assertEquals(0.75f, MatchedUtils.getSimilarity("abcd", "abcc", levenstein), 0.0f);
        assertEquals(0.75f, MatchedUtils.getSimilarity("abcd", "abc", levenstein), 0.0f);
        assertEquals(0.0f, MatchedUtils.getSimilarity("abcd", "efgh", levenstein), 0.0f);
        
        //in case that one or both of comapared strings are null returns 0.5
        assertEquals(0.5f, MatchedUtils.getSimilarity(null, "abc", levenstein), 0.0f);
        assertEquals(0.5f, MatchedUtils.getSimilarity("abc", null, levenstein), 0.0f);
        assertEquals(0.5f, MatchedUtils.getSimilarity(null, null, levenstein), 0.0f);
    }
    
    /**
     * Tests for MatchedUtils#getSimilarities(java.lang.Object, java.util.List, java.util.function.BiFunction) function.
     */
    @Test
    public void getSimilaritiesTest() {
        final List<String> targets = Arrays.asList("abcd", "abcc", "abc", "efgh", null);
        
        final List<Float> expect = Arrays.asList(1f, 0.75f, 0.75f, 0f, 0.5f);
        
        assertArrayEquals(
            expect.stream().mapToDouble((i) -> i).toArray(),
            MatchedUtils.getSimilarities(targets.get(0), targets, levenstein).stream().mapToDouble((i) -> i).toArray(),
            0f);
    }
    
    /**
     * Tests for MatchedUtils#trigramSimilarity(java.lang.Object, java.lang.Object) function.
     */
    @Test
    public void trigramSimilarityTest() {
        assertEquals(1f, MatchedUtils.trigramSimilarity("abc", "abc"), 0f);        
        assertEquals(new NGramDistance(3).getDistance("abc123", "ab c123"),
            MatchedUtils.trigramSimilarity("abc123", "ab c123"), 0f);        
        assertEquals(0f, MatchedUtils.trigramSimilarity("abc", "efg"), 0f);
        
        //in case that one or both of compared strings are null returns 0.5
        assertEquals(0.5f, MatchedUtils.trigramSimilarity("abc123", null), 0f);
        assertEquals(0.5f, MatchedUtils.trigramSimilarity(null, "ab c123"), 0f);
        assertEquals(0.5f, MatchedUtils.trigramSimilarity(null, null), 0f);
    }
    
    /**
     * Tests for MatchedUtils#oneDigitSimilarity(java.lang.Object, java.lang.Object) function.
     */
    @Test
    public void oneDigitSimilarityTest() {
        assertEquals(1f, MatchedUtils.oneDigitSimilarity("abc", "abc"), 0f);
        assertEquals(0.8f, MatchedUtils.oneDigitSimilarity("abc", "abb"), 0f);
        //more than one different character
        assertEquals(0f, MatchedUtils.oneDigitSimilarity("abc", "efg"), 0f);
        assertEquals(0f, MatchedUtils.oneDigitSimilarity("abc", "bbb"), 0f);
        
        //in case that one or both of compared strings are null returns 0.5
        assertEquals(0.5f, MatchedUtils.oneDigitSimilarity("abc123", null), 0f);
        assertEquals(0.5f, MatchedUtils.oneDigitSimilarity(null, "ab c123"), 0f);
        assertEquals(0.5f, MatchedUtils.oneDigitSimilarity(null, null), 0f);
    }
    
    /**
     * Tests for MatchedUtils#nutsSimilarities(java.lang.Object, java.util.List) function.
     */
    @Test
    public void nutsSimilaritiesTest() {
        final List<String> targets = Arrays.asList("CZ041", "CZ042", "CZ04", "SK012", null);
        
        final List<Float> expect = Arrays.asList(1f, 0.8f, 0f, 0f, 0.5f);
        
        assertArrayEquals(
            expect.stream().mapToDouble((i) -> i).toArray(),
            MatchedUtils.nutsSimilarities(targets.get(0), targets, 0).stream().mapToDouble((i) -> i).toArray(),
            0f);
    }
    
    /**
     * Tests for MatchedUtils#nutsSimilarities(java.lang.Object, java.util.List) function with set crop of nuts codes.
     */
    @Test
    public void nutsSimilaritiesWithCropTest() {
        final List<String> targets = Arrays.asList("CZ041", "CZ042", "CZ04", "SK012", null);
        
        final List<Float> expect = Arrays.asList(1f, 1f, 1f, 0f, 0.5f);
        
        assertArrayEquals(
            expect.stream().mapToDouble((i) -> i).toArray(),
            MatchedUtils.nutsSimilarities(targets.get(0), targets, 4).stream().mapToDouble((i) -> i).toArray(),
            0f);
    }
    
    /**
     * Tests for MatchedUtils#bodyIdSimilarities(eu.dl.dataaccess.dto.codetables.BodyIdentifier, java.util.List)
     * function.
     */
    @Test
    public void bodyIdSimilaritiesTest() {
        final List<BodyIdentifier> targets = Arrays.asList(
            new BodyIdentifier()
                .setId("123").setScope(BodyIdentifier.Scope.CZ).setType(BodyIdentifier.Type.VAT),
            new BodyIdentifier()
                .setId("123").setScope(BodyIdentifier.Scope.CZ).setType(BodyIdentifier.Type.HEADER_ICO),
            new BodyIdentifier()
                .setId("133").setScope(BodyIdentifier.Scope.CZ),
            new BodyIdentifier()
                .setId("333").setScope(BodyIdentifier.Scope.CZ),
            new BodyIdentifier()
                .setId("123").setScope(BodyIdentifier.Scope.SK),
            null
        );

        // result list includes only similarities for comparable body identifiers comparable
        final List<Float> expect = Arrays.asList(1f, 1f, 0.8f, 0f);
        
        assertArrayEquals(
            expect.stream().mapToDouble((i) -> i).toArray(),
            MatchedUtils.bodyIdSimilarities(targets.get(0), targets).stream().mapToDouble((i) -> i).toArray(),
            0f);
    }
    
    /**
     * Tests for MatchedUtils#levensteinDistanceFromSimilarity(java.lang.String, java.lang.String, java.lang.Float)
     * function.
     */
    @Test
    public void levensteinDistanceFromSimilarity() {        
        assertEquals(0, MatchedUtils.levensteinDistanceFromSimilarity("abc", "abc", levenstein.apply("abc", "abc")));
        assertEquals(1, MatchedUtils.levensteinDistanceFromSimilarity("abc", "abb", levenstein.apply("abc", "abb")));
        assertEquals(4, MatchedUtils.levensteinDistanceFromSimilarity("abc", "efgh", levenstein.apply("abc", "efgh")));
    }

    /**
     * Tests for {@link MatchedUtils#areBodyIdsComparable(eu.dl.dataaccess.dto.codetables.BodyIdentifier,
     * eu.dl.dataaccess.dto.codetables.BodyIdentifier)}
     * and {@link MatchedUtils#isBodyIdValidForComparsion(eu.dl.dataaccess.dto.codetables.BodyIdentifier) } functions.
     */
    @Test
    public void bodyIdentifiersComparabilityTest() {
        // valid body indentifier - scope and id not null
        BodyIdentifier valid = new BodyIdentifier().setId("123").setScope(BodyIdentifier.Scope.CZ);
        // invalid body - scope or id is null
        BodyIdentifier invalid = new BodyIdentifier().setScope(BodyIdentifier.Scope.CZ);
        // identifier incomparable with valid (above) - has different scope
        BodyIdentifier incomparable = new BodyIdentifier().setId("123").setScope(BodyIdentifier.Scope.SK);
        // identifier comparable with valid (above) - has same scope and is valid for comparsion
        BodyIdentifier comparable = new BodyIdentifier().setId("321").setScope(BodyIdentifier.Scope.CZ);


        assertTrue(MatchedUtils.isBodyIdValidForComparsion(valid));
        assertTrue(MatchedUtils.isBodyIdValidForComparsion(incomparable));
        assertTrue(MatchedUtils.isBodyIdValidForComparsion(comparable));
        assertTrue(MatchedUtils.areBodyIdsComparable(valid, comparable));
        
        assertFalse(MatchedUtils.isBodyIdValidForComparsion(invalid));
        assertFalse(MatchedUtils.areBodyIdsComparable(valid, invalid));
        assertFalse(MatchedUtils.areBodyIdsComparable(valid, incomparable));
    }
}
