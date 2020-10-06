package eu.dl.worker.utils.matched;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.BiFunction;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.apache.lucene.search.spell.LevensteinDistance;
import org.apache.lucene.search.spell.NGramDistance;

import eu.dl.dataaccess.dto.codetables.BodyIdentifier;

/**
 * Class provides useful functions for matching.
 * 
 * @author Tomas Mrazek
 */
public final class MatchedUtils {
    
    /**
     * Suppress default constructor for noninstantiability.
     */
    private MatchedUtils() {
        throw new AssertionError();
    }
    
    /**
     * Computes similarity for two objects of the same class. Proceeds according to the following algorithm:
     * 
     * <ul>
     *      <ol>= 1     - perfect match</ol>
     *      <ol>= 0.5   - {@code source} or {@code target} is null</ol>
     *      <ol>= ?     - output of {@code processor}</ol>
     * </ul>
     * 
     * @param <S>
     *      compared items class
     * @param source
     *      source item for comparsion
     * @param target
     *      target item for comparsion
     * @param processor
     *      function that accepts two parameters of S class and returns Float from 0-1 (similarity)
     * @return similarity
     */
    public static <S> Float getSimilarity(final S source, final S target, final BiFunction<S, S, Float> processor) {
        if (source == null || target == null) {
            return 0.5f;
        } else if (source.equals(target)) {
            return 1f;
        }

        return processor.apply(target, source);
    }
    
    /**
     * Computes similarity for the given {@code source} object with each object from {@code target} list.
     * 
     * @see MatchedUtils#getSimilarity(java.lang.Object, java.lang.Object, java.util.function.BiFunction)
     * 
     * @param <S>
     *      compared items class
     * @param source
     *      source item for comparsion
     * @param targets
     *      list of target items for comparsion
     * @param processor
     *      function that accepts two parameters of S class and returns Float from 0-1 (similarity)
     * @return list of similarities
     */
    public static <S> List<Float> getSimilarities(final S source, final List<S> targets,
        final BiFunction<S, S, Float> processor) {

        if (source == null || targets == null) {
            return Arrays.asList(0.5f);
        } else if (source.equals(targets)) {
            return Arrays.asList(1.0f);
        }

        final List<Float> similarity = new ArrayList<>();
        targets.forEach((t) -> similarity.add(getSimilarity(source, t, processor)));
        
        return similarity;
    }
    
    /**
     * Computes similarity of two given strings with help of trigram matching algorithm.
     * 
     * @see NGramDistance#getDistance(java.lang.String, java.lang.String)
     * @see MatchedUtils#getSimilarity(java.lang.Object, java.lang.Object, java.util.function.BiFunction)
     * 
     * @param source
     *      source string
     * @param target
     *      target string
     * @return similarity of strings
     */
    public static Float trigramSimilarity(final String source, final String target) {
        return getSimilarity(source, target, (s, t) -> new NGramDistance(3).getDistance(s, t));
    }
    
    /**
     * Computes similarity of two given strings. Algorithm works as follows:
     * 
     * <ul>
     *      <ol>= 1     - perfect match</ol>
     *      <ol>= 0.5   - difference in one character</ol>
     *      <ol>= 0     - otherwise</ol>
     * </ul>
     * 
     * @see MatchedUtils#getSimilarity(java.lang.Object, java.lang.Object, java.util.function.BiFunction)
     * 
     * @param source
     *      source string
     * @param target
     *      target string
     * @return similarity of strings 
     */
    public static Float oneDigitSimilarity(final String source, final String target) {
        return getSimilarity(source, target, (s, t) -> {
            final LevensteinDistance ld = new LevensteinDistance();
            final float similarity = ld.getDistance(s, t);

            final int distance = levensteinDistanceFromSimilarity(source, target, similarity);
            
            if (distance == 0) {
                return 1f;
            } else if (distance == 1) {
                return 0.8f;
            }

            return 0f;
        });
    }
    
    /**
     * Levenstein similarity of two given strings expressed as distance.
     * 
     * @param source
     *      source string
     * @param target
     *      target string
     * @param similarity
     *      similarity of strings as a number from 1-0
     * @return distance of two given strings
     */
    public static int levensteinDistanceFromSimilarity(final String source, final String target,
        final Float similarity) {
        
        return Math.round(Math.max(source.length(), target.length()) * (1 - similarity));
    }
    
    /**
     * Computes similarities of {@code source} body identifier with each comparable item from {@code targets} list.
     * 
     * @see MatchedUtils#oneDigitSimilarity(java.lang.String, java.lang.String)
     * @see MatchedUtils#getSimilarities(java.lang.Object, java.util.List, java.util.function.BiFunction)
     * @see MatchedUtils#areBodyIdsComparable(eu.dl.dataaccess.dto.codetables.BodyIdentifier,
     *  eu.dl.dataaccess.dto.codetables.BodyIdentifier)
     * 
     * @param source
     *      source nuts code
     * @param targets
     *      list of target nuts codes
     * @return similarities of {@code source} with each item from {@code targets}
     */
    public static List<Float> bodyIdSimilarities(final BodyIdentifier source, final List<BodyIdentifier> targets) {
        List<BodyIdentifier> comparableTargets =
            targets.stream().filter(t -> areBodyIdsComparable(t, source)).collect(Collectors.toList());

        return getSimilarities(source, comparableTargets, (s, t) -> {
            return oneDigitSimilarity(s.getId(), t.getId());
        });
    }

    /**
     * @param source
     *      source body identifier
     * @param target
     *      target body identifier
     * @return true only and only if the both identifiers are valid for comparsion and have same scope.
     */
    public static boolean areBodyIdsComparable(final BodyIdentifier source, final BodyIdentifier target) {
        return isBodyIdValidForComparsion(source) && isBodyIdValidForComparsion(target)
            && source.getScope() == target.getScope();
    }

    /**
     * @param bodyId
     *      body identifier
     * @return true only and only if the body identifier is not null and its id and scope are also not null
     */
    public static boolean isBodyIdValidForComparsion(final BodyIdentifier bodyId) {
        return bodyId != null && bodyId.getId() != null && bodyId.getScope() != null;
    }

    /**
     * Computes similarities for {@code source} nuts code with each item from {@code targets} list. Algorithm works as
     * follows:
     * 
     * <ul>
     *      <ol>= 1     - perfect match</ol>
     *      <ol>= 0.5   - difference in last character in case of 5 digit code</ol>
     *      <ol>= 0     - otherwise</ol>
     * </ul>
     * 
     * @see MatchedUtils#getSimilarities(java.lang.Object, java.util.List, java.util.function.BiFunction) 
     * 
     * @param source
     *      source nuts code
     * @param targets
     *      target nuts code
     * @param cropNuts
     *      number of the most left characters of nuts used for matching, if is 0 orginal codes are comapred
     * @return similarities of nuts codes
     */
    public static List<Float> nutsSimilarities(final String source, final List<String> targets, final int cropNuts) {
        return getSimilarities(source, targets, (s, t) -> {
            final LevensteinDistance ld = new LevensteinDistance();        
            float similarity = ld.getDistance(
                cropNuts > 0 ? StringUtils.left(s, cropNuts) : s,
                cropNuts > 0 ? StringUtils.left(t, cropNuts) : t);
            
            final int distance  = levensteinDistanceFromSimilarity(s, t, similarity);
            
            if (distance == 0) {
                return 1f;
            } else if (distance == 1 && s.length() == 5 && s.substring(0, 4).equals(t.substring(0, 4))) {
                return 0.8f;
            }
            
            return 0f;            
        });
    }
}
