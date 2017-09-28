package eu.dl.worker.matched.plugin;

import eu.dl.dataaccess.dao.ExactMatchBodyDAO;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;


import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.matched.ExactellyMatchable;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.worker.utils.ArrayUtils;

/**
 * This plugin attempts to find exact match with pool of bodies.
 * <p>
 * <p>Body is compared to each matched item, matches of following are checked:</p>
 * <ul>
 * <li>standardized name</li>
 * <li>standardized address</li>
 * <li>all available identifiers (matching only against the same type of identifier minding its scope)</li>
 * </ul>
 * <p>
 * <p>Perfect match of at least two not-NULL items is considered exact match (for instance std. name + identifier,
 * two different identifiers, identifier + std. address etc.). If such match occurs, Body is assigned as a member of
 * group and matching ends.</p>
 *
 * @param <T>
 *         class of the matched body
 * @param <U>
 *         class of the body from the pool
 */
public abstract class BaseExactMatchingPlugin<T extends MatchedBody, U extends ExactellyMatchable> extends
        BaseBodiesPoolMatchingPlugin<T, U, ExactMatchBodyDAO> {

    private static final int MATCH_SCORE_LIMIT = 2;

    /**
     * Constructor with body pool DAO initialization.
     *
     * @param poolDAO
     *      body pool DAO
     */
    public BaseExactMatchingPlugin(final ExactMatchBodyDAO poolDAO) {
        super(poolDAO);
    }

    @Override
    protected final Map<U, Float> getSimiliraties(final T item, final List<U> pool) {
        final HashMap<U, Float> scoreBoard = new HashMap<>();

        pool.forEach(body -> {
            float score = 0;

            if (body.getStandardizedName() != null && item.getStandardizedName() != null
                    && body.getStandardizedName().equals(item.getStandardizedName())) {
                score++;
            }

            if (body.getStandardizedAddress() != null && item.getStandardizedAddress() != null
                    && body.getStandardizedAddress().equals(item.getStandardizedAddress())) {
                score++;
            }

            if (body.getBodyIds() != null && item.getBodyIds() != null) {
                List<BodyIdentifier> bodyIds = body.getBodyIds().stream()
                    .filter(id -> id != null && id.getId() != null && id.getScope() != null)
                    .filter(ArrayUtils.distinct(n -> n.getId() + n.getScope())).collect(Collectors.toList());

                List<BodyIdentifier> itemBodyIds = item.getBodyIds().stream()
                    .filter(id -> id != null && id.getId() != null && id.getScope() != null)
                    .filter(ArrayUtils.distinct(n -> n.getId() + n.getScope())).collect(Collectors.toList());

                for (BodyIdentifier bodyId : bodyIds) {
                    for (BodyIdentifier itemId : itemBodyIds) {
                        if (bodyId.getId().equals(itemId.getId()) && bodyId.getScope().equals(itemId.getScope())) {
                            score++;
                        }
                    }
                }
            }

            if (score >= MATCH_SCORE_LIMIT) {
                scoreBoard.put(body, score);
            }
        });

        return scoreBoard;
    }

    /**
     * Returns the pool of bodies for exact matching of the given {@code item}. The pool is identified as union of these
     * three:
     * <ul>
     * <ol>bodies with same standardized name as the given {@code item}</ol>
     * <ol>bodies with same standardized address as the given {@code item}</ol>
     * <ol>bodies whose body identifiers have non-empty intersection with body identifiers of the given
     * {@code item}</ol>
     * </ul>
     *
     * @param item
     *         matched item
     *
     * @return list of bodies
     */
    @Override
    public final List<U> getBodiesPool(final T item) {
        return poolDAO.getExactMatchBodiesPool(item.getStandardizedName(),
            item.getStandardizedAddress(), item.getBodyIds());
    }
}
