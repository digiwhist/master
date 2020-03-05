package eu.datlab.worker.py.matched;

import eu.dl.dataaccess.dao.MatchedBodyDAO;
import eu.dl.dataaccess.dto.codetables.BodyType;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.worker.matched.plugin.MatchingPlugin;
import eu.dl.worker.matched.plugin.MatchingResult;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.List;

/**
 * Paraguay buyers matching plugin.
 */
public class DNCPBuyersPlugin implements MatchingPlugin<MatchedBody> {

    protected final Logger logger = LoggerFactory.getLogger(this.getClass().getName());

    private static final String MATCHED_BY = "DNCPBuyersPlugin";

    private final MatchedBodyDAO dao;

    /**
     * Constructor with body DAO initialization.
     *
     * @param dao
     *      body dao
     */
    public DNCPBuyersPlugin(final MatchedBodyDAO<MatchedBody> dao) {
        this.dao = dao;
    }

    @Override
    public final MatchingResult match(final MatchedBody item) {
        final MatchingResult matchingResult = new MatchingResult();

        if (item.getRole() != BodyType.BUYERS) {
            return matchingResult;
        }

        final List<MatchedBody> matchedPoolBodies = dao.getByNameAndRole(item.getName(), item.getRole());

        if (!matchedPoolBodies.isEmpty()) {
            MatchedBody best = matchedPoolBodies.get(0);

            HashMap<String, Object> meta = new HashMap<>();
            meta.put("matchingScore", 1.0);
            meta.put("pairedBodyId", best.getId());

            matchingResult.setGroupId(best.getGroupId());
            matchingResult.setMatched(true);
            matchingResult.setMatchedBy(MATCHED_BY);
            matchingResult.setMetaData(meta);
            matchingResult.setMatchedBody(best);
        }

        return matchingResult;
    }
}
