package eu.dl.worker.matched.plugin;

import eu.dl.dataaccess.dao.ApproximateMatchBodyDAO;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.matched.ApproximatellyMatchable;
import eu.dl.dataaccess.dto.matched.EtalonBody;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.worker.utils.matched.MatchedUtils;

/**
 * This plugin attempts to find approximate match with pool of bodies. 
 *
 * The pool of bodies for approximate matching is identified as union of these two:
 * <ul>
 *      <ol>a) all matches already found with help of Exact matching with already matched items, with one perfectly
 *          matched field</ol>
 *      <ol>b) all bodies within existing groups with perfectly matching digest</ol>
 * </ul>
 * 
 * 
 * The plugin computes match {@code S_i} of Body with each member of pool {@code i}, as number from 0-1. Comparison of
 * attribute pair with one or both values NULL automatically results in value 0.5.
 *
 * <ul>
 *      <li>standardized name (trigram matching), weight 1</li>
 *      <li>standardized address (trigram matching), weight 1</li>
 *      <li>id match (1 for perfect match, 0.8 for difference in one digit, 0 otherwise), weight 1</li>
 *      <li>postcode (1 for perfect match, 0.5 for difference in one digit, 0 otherwise), weight 0.2</li>
 *      <li>nuts (1 for perfect match, 0.8 for difference in last digit in case of 5 digit code like CZ041,
 *          0 otherwise), weight 0.2</li>
 * </ul>
 * 
 * finally, the S_i = weighted average of all match ratios
 *
 * If some S_i > 0.75 (arbitrary threshold, will be subject to change :D, this one has been set as rough equivalent of
 * two exact matches in 1), we take the match with max_i(S_i) and matching ends.
 * 
 * @param <T>
 *      class of the matched body
 * @param <U>
 *      class of the body from the pool
 */
public abstract class BaseApproximateMatchingPlugin<T extends MatchedBody, U extends ApproximatellyMatchable>
    extends BaseBodiesPoolMatchingPlugin<T, U, ApproximateMatchBodyDAO> {
        
    private static final float MATCH_LOWER_THRESHOLD = 0.75f;

    /**
     * Constructor with body pool DAO initialization.
     *
     * @param poolDAO
     *      approximate body pool DAO
     */
    public BaseApproximateMatchingPlugin(final ApproximateMatchBodyDAO poolDAO) {
        super(poolDAO);
    }

    @Override
    protected final Map<U, Float> getSimiliraties(final T item, final List<U> pool) {
        final Map<U, Float> bodySimilarities = new HashMap<>();

        pool.forEach((body) -> {
            float nameSim =
                MatchedUtils.trigramSimilarity(item.getStandardizedName(), body.getStandardizedName());

            float addrSim =
                MatchedUtils.trigramSimilarity(item.getStandardizedAddress(), body.getStandardizedAddress());

            final Float bodyIdSim = getBestBodyIdSimilarity(item.getBodyIds(), body.getBodyIds());

            float postcodeSim = getPostcodeSimilarity(item.getPostcode(), body.getPostcode());

            final Float nutsSim =
                getBestNutsSimilarity(item.getNuts(), body.getNuts(), (body instanceof EtalonBody ? 5 : 0));

            float weightenedAverage = (
                nameSim
                + addrSim
                + bodyIdSim
                + postcodeSim * 0.2f
                + nutsSim * 0.2f
                ) / (1 + 1 + 1 + 0.2f + 0.2f);

            if (weightenedAverage >= MATCH_LOWER_THRESHOLD) {
                bodySimilarities.put(body, weightenedAverage);
            }
        });

        return bodySimilarities;
    }
    
    /**
     * Returns the pool of bodies for approximate matching of the given {@code item}. The pool is identified as union of
     * these two:
     * <ul>
     *      <ol>all matches already found with help of Exact matching with already matched items, with one perfectly
     *      matched field</ol>
     *      <ol>all bodies within existing groups with perfectly matching digest</ol>
     * </ul>
     * 
     * @param item
     *      macthed item
     * @return list of bodies
     */
    @Override
    protected abstract List<U> getBodiesPool(T item);
    
    /**
     * Computes similarities for each BodyIdentifier from {@code source} with each BodyIdentifier from {@target}
     * and returns the best match.
     * 
     * @param source
     *      origin list of bodies identifiers
     * @param target
     *      list of bodies identifiers that is compared with {@code source}
     * @return best match or null
     */
    private Float getBestBodyIdSimilarity(final List<BodyIdentifier> source, final List<BodyIdentifier> target) {
        if (source == null || target == null || source.isEmpty() || target.isEmpty()) {
            return 0.5f;
        }

        // test whether source and target list contains some comparable items, if not return 0.5
        if (!source.stream().anyMatch(s -> target.stream().anyMatch(t -> MatchedUtils.areBodyIdsComparable(s, t)))) {
            return 0.5f;
        }

        final List<Float> similarities = new ArrayList<>();        
        source.forEach((bodyId) -> {
            similarities.addAll(MatchedUtils.bodyIdSimilarities(bodyId, target));
        });
        
        return (similarities.isEmpty() ? 0f : Collections.max(similarities));
    }
    
    /**
     * Computes similarity for postcode from {@code source} and {@code target} item.
     * 
     * @param source
     *      origin matched postcode
     * @param target
     *      postcode that is compared with {@code source}
     * @return similarity of postocodes or 0.5 in case that on of compared items haven't defined postcode
     */
    private Float getPostcodeSimilarity(final String source, final String target) {
        return MatchedUtils.oneDigitSimilarity(source, target);
    }
    
    /**
     * Computes similarities for each nuts from {@code source} with each nuts from {@target} and returns the best match.
     * 
     * @param source
     *      origin list of NUTS codes
     * @param target
     *      list of NUTS codes that are compared with {@code source}
     * @param trimNuts
     *      number of the most left characters of nuts used for matching, if is 0 orginal codes are comapred
     * @return best match
     */
    private Float getBestNutsSimilarity(final List<String> source, final List<String> target, final int trimNuts) {
        if (source == null || target == null) {            
            return 0.5f;
        }
        
        final List<Float> similarities = new ArrayList<>();        
        source.forEach((code) -> {
            similarities.addAll(MatchedUtils.nutsSimilarities(code, target, trimNuts));
        });
        
        return (similarities.isEmpty() ? 0f : Collections.max(similarities));
    }
}
