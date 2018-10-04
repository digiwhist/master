package eu.dl.worker.matched.plugin;

import eu.dl.dataaccess.dao.MatchedBodyDAO;
import eu.dl.dataaccess.dao.ApproximateMatchBodyDAO;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import java.util.List;

/**
 * This plugin attempts to find approximate match with already matched bodies.
 * 
 * @see BaseApproximateMatchingPlugin
 * 
 * @param <T>
 *      class of the matched body
 */
public class ApproximateMatchingPlugin<T extends MatchedBody> extends BaseApproximateMatchingPlugin<T, T> {
    private final MatchedBodyDAO matchedDAO;
    
    private static final String MATCHED_BY = "approximate";

    /**
     * Approximate matching plugin with matched body DAO initialization.
     *
     * @param matchedBodyDAO 
     *      matched body DAO
     */
    public ApproximateMatchingPlugin(final MatchedBodyDAO matchedBodyDAO) {
        super(matchedBodyDAO);
        this.matchedDAO = matchedBodyDAO;
    }

    @Override
    protected final  String getMatchedBy() {
        return MATCHED_BY;
    }
    
    @Override
    protected final List<T> getBodiesPool(final T item) {
        return ((ApproximateMatchBodyDAO) poolDAO).getApproximateMatchBodiesPool(item.getStandardizedName(),
            item.getStandardizedAddress(), item.getBodyIds(), item.getDigest());
    }

    @Override
    protected final MatchedBody bestToMatchedBody(final T best) {
        return best;
    }
}
