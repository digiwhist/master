package eu.dl.worker.matched.plugin;

import eu.dl.dataaccess.dao.MatchedBodyDAO;
import eu.dl.dataaccess.dto.matched.MatchedBody;

/**
 * This plugin attempts to find exact match with already matched bodies. 
 * 
 * <p>Body is compared to each matched item, matches of following are checked:</p>
 * <ul>
 *      <li>standardized name</li>
 *      <li>standardized address</li>
 *      <li>all available identifiers (matching only against the same type of identifier minding its scope)</li>
 * </ul>
 * 
 * <p>Perfect match of at least two not-NULL items is considered exact match (for instance std. name + identifier,
 * two different identifiers, identifier + std. address etc.). If such match occurs, Body is assigned as a member of
 * group and matching ends.</p>
 * 
 * @param <T>
 *      class of the matched body
 */
public class ExactMatchingPlugin<T extends MatchedBody>  extends BaseExactMatchingPlugin<T, T> {
    private static final String MATCHED_BY = "exact";
    
    /**
     * Exact matching plugin with matched and etalon body DAO initialization.
     * 
     * @param matchedBodyDAO 
     *      matched body DAO
     */
    public ExactMatchingPlugin(final MatchedBodyDAO matchedBodyDAO) {
        super(matchedBodyDAO);
    }
    
    @Override
    protected final  String getMatchedBy() {
        return MATCHED_BY;
    }
    
    @Override
    protected final MatchedBody bestToMatchedBody(final T best) {
        return best;
    }
}
