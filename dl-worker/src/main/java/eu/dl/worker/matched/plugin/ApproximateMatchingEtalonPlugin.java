package eu.dl.worker.matched.plugin;

import java.util.Collections;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import eu.dl.dataaccess.dao.EtalonBodyDAO;
import eu.dl.dataaccess.dao.MatchedBodyDAO;
import eu.dl.dataaccess.dao.ApproximateMatchBodyDAO;
import eu.dl.dataaccess.dto.matched.EtalonBody;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.utils.DigestUtils;

/**
 * This plugin attempts to find approximate match with already etalon bodies. 
 *
 * @param <T>
 *      class of the matched body
 * @param <U>
 *      class of the body from the pool
 */
public class ApproximateMatchingEtalonPlugin<T extends MatchedBody, U extends EtalonBody>
    extends BaseApproximateMatchingPlugin<T, U> {
    
    private final MatchedBodyDAO matchedDAO;

    private final String sourceId;

    /**
     * Value used for positive matching result.
     */
    public static final String MATCHED_BY = "approximateEtalon";

    private static final Logger logger = LoggerFactory.getLogger(ApproximateMatchingEtalonPlugin.class);

    /**
     * Approximate matching plugin with matched and etalon body DAO initialization.
     * 
     * @param matchedBodyDAO 
     *      matched body DAO
     * @param etalonBodyDAO 
     *      etalon body DAO
     * @param sourceId
     *      ID of source
     */
    public ApproximateMatchingEtalonPlugin(final MatchedBodyDAO matchedBodyDAO, final EtalonBodyDAO etalonBodyDAO,
                                           final String sourceId) {
        super(etalonBodyDAO);
        this.matchedDAO = matchedBodyDAO;
        this.sourceId = sourceId;
    }
    
    @Override
    protected final  String getMatchedBy() {
        return MATCHED_BY;
    }
    
    @Override
    protected final List<U> getBodiesPool(final T item) {
        if (poolDAO == null) {
            return Collections.emptyList();
        }
        
        List approximateMatchBodiesPool =
            ((ApproximateMatchBodyDAO) poolDAO).getApproximateMatchBodiesPool(item.getStandardizedName(),
                item.getStandardizedAddress(), item.getBodyIds(), item.getDigest());

        if (approximateMatchBodiesPool.size() > 1000) {
            logger.warn("Approximate etalon body records returned from for diget '{}': {}",
                item.getDigest(), approximateMatchBodiesPool.size());
        }
        
        return approximateMatchBodiesPool;
    }

    @Override
    protected final MatchedBody bestToMatchedBody(final U best) {
        // search for already existing etalon entry stored in the matched body
        // collection
        T matched = (T) matchedDAO.getByEtalonId(best.getId());
        if (matched != null) {
            return matched;
        }

        //new MatchedBody from Etalon item
        matched = (T) best.getAsMatchedBody();
        String groupId = "group_" + sourceId + "_body_" + DigestUtils.bodyHash(matched);
        matched.setGroupId(groupId);

        String id = matchedDAO.save(matched);
        matched.setId(id);

        return matched;
    }
}
