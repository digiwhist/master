package eu.dl.worker.matched.plugin;

import eu.dl.dataaccess.dao.EtalonBodyDAO;
import eu.dl.dataaccess.dao.MatchedBodyDAO;
import eu.dl.dataaccess.dto.matched.EtalonBody;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.utils.DigestUtils;

/**
 * This plugin attempts to find exact match with already matched bodies.
 * 
 * @see BaseExactMatchingPlugin
 * 
 * @param <T>
 *      class of the matched body
 * @param <U>
 *      class of the body from the pool
 */
public class ExactMatchingEtalonPlugin<T extends MatchedBody, U extends EtalonBody>
    extends BaseExactMatchingPlugin<T, U> {

    private final MatchedBodyDAO matchedDAO;
    
    private final String sourceId;
    
    /**
     * Value used for positive matching result.
     */
    public static final String MATCHED_BY = "exactEtalon";
    
    /**
     * Exact matching plugin with matched and etalon body DAO initialization.
     * 
     * @param matchedBodyDAO 
     *      matched body DAO
     * @param etalonBodyDAO 
     *      etalon body DAO
     * @param sourceId
     *      ID of source
     */
    public ExactMatchingEtalonPlugin(final MatchedBodyDAO matchedBodyDAO, final EtalonBodyDAO etalonBodyDAO,
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
