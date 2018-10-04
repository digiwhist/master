package eu.dl.worker.utils;

import java.util.ArrayList;
import java.util.List;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Matches object T with list of objects U (pool) and returns best match.
 *
 * @param <T>
 *      left-side item class
 * @param <U>
 *      right-side item class
 * @author Tomas Mrazek
 */
public abstract class BaseObjectMatcher<T, U> {
    protected final Logger logger = LoggerFactory.getLogger(this.getClass().getName());

    /**
     * Default constructor.
     */
    public BaseObjectMatcher() {
    }

    /**
     * Calculates score for each item from the pool and selects best match.
     *
     * @param input
     *      matched item
     * @return best match or null if no match was found
     */
    public final U match(final T input) {
        if (input == null) {
            return null;
        }

        U bestMatch = preprocess(input);
        if (bestMatch != null) {
            logger.info("Best match {} for {} found in preprocessing", bestMatch, input);
            return bestMatch;
        }

        List<U> pool = getPool(input);
        if (pool == null || pool.isEmpty()) {
            logger.info("No match found for {} because of pool is empty", input);
            return null;
        }

        logger.info("Pool of {} entries retrieved", pool.size());

        List<Pair<U, Float>> scoreboard = new ArrayList<>();
        pool.forEach(n -> {
            Float score = getScore(input, n);
            if (score != null) {
                scoreboard.add(Pair.of(n, score));
            }
        });

        Pair<U, Float> bestMatchWithScore = scoreboard.stream().max((a, b) -> Float.compare(a.getValue(), b.getValue())).orElse(null);
        if (bestMatchWithScore == null) {
            logger.info("No match found for {}.", input);
            return null;
        }
        bestMatch = postprocess(input, bestMatchWithScore.getKey());

        logger.info("Best match {} found for {} with score {}", bestMatchWithScore.getKey(), input, bestMatchWithScore.getValue());

        return bestMatch;
    }

    /**
     * Preprocessing of the input. Except the preprocessing an input it is possible to use this function for other purposes eg. retrieving
     * best match from some cache. In case that this function returns non-null output (best match), the matching process
     * ({@link BaseObjectMatcher#match(java.lang.Object)}) ends and returns this output.
     *
     * @param input
     *      matched item
     * @return best match or null
     */
    protected abstract U preprocess(T input);

    /**
     * Postprocessing of the best match. This is the last step before commit and best match returning. So this function can be used
     * for adjustment of the best match but also for other implementation-specific purposes eg. saving of the best match to some cache.
     *
     * @param input
     *      matched item
     * @param bestMatch
     *      best match for input
     * @return best match
     */
    protected abstract U postprocess(T input, U bestMatch);

    /**
     * Returns list of items which are matched with input.
     *
     * @param input
     *      matched item
     * @return list of items where the best match is looked for
     */
    protected abstract List<U> getPool(T input);

    /**
     * Calculates score for input items.
     *
     * @param t
     *      master item
     * @param u
     *      the item for which the score is calculated
     * @return score or NULL (such items are excluded from matching process)
     */
    public abstract Float getScore(T t, U u);
}
