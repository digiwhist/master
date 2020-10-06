package eu.datlab.worker.matched;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.MatchedTenderDAO;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.worker.matched.plugin.MatchingResult;
import eu.dl.worker.matched.plugin.TenderMatchingPlugin;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Base matching plugin for tenders.
 *
 * @author Tomas Mrazek
 */
public abstract class BaseTenderMatchingPlugin implements TenderMatchingPlugin {
    protected final MatchedTenderDAO matchedTenderDao;

    protected final boolean isStrict;

    protected final Logger logger = LoggerFactory.getLogger(this.getClass().getName());

    /**
     * Plugin needs MatchedTenderDAO and allows to enable/disable strict mode. Strict mode controls behavior of the
     * plugin in cases when matched tenders which were returned with
     * {@link getMatchedTender(eu.dl.dataaccess.dto.matched.MatchedTender) } appear in more than one group. If true, all
     * matched tenders have to be from the same group otherwise, throws UnrecoverableException. If a false, the same
     * group is set for the each item from the matched list.
     *
     * @param dao
     *         matched tender DAO
     * @param isStrict
     *          enables/disables strict mode
     */
    BaseTenderMatchingPlugin(final MatchedTenderDAO dao, final boolean isStrict) {
        this.matchedTenderDao = dao;
        this.isStrict = isStrict;
    }

    /**
     * Plugin needs MatchedTenderDAO. Strict mode is turn on.
     *
     * @param dao
     *         matched tender DAO
     */
    BaseTenderMatchingPlugin(final MatchedTenderDAO dao) {
        this.matchedTenderDao = dao;
        this.isStrict = true;
    }

    /**
     * @return name of the matching plugin
     */
    public abstract String getMatchedBy();

    /**
     * Returns list of matched tenders for matching with the given {@code matchedTender}.
     * 
     * @param matchedTender
     *      matched tender
     * @return list of matched tenders
     */
    public abstract List<MatchedTender> getMatchedTenders(MatchedTender matchedTender);

    @SuppressWarnings("unchecked")
    @Override
    public final MatchingResult match(final MatchedTender matchedTender) {
        final MatchingResult matchingResult = new MatchingResult();

        final List<MatchedTender> matchedTenders = getMatchedTenders(matchedTender);

        if (matchedTenders != null && !matchedTenders.isEmpty()) {
            String groupId = matchedTenders.get(0).getGroupId();

            for (MatchedTender tender : matchedTenders) {
                if (!tender.getGroupId().equals(groupId)) {
                    if (isStrict) {
                        // we matched group ID through another publication -> group ID has to be same in strict mode
                        logger.error("Two forms referenced the actual form are from different groups"
                            + " (IDs of groups are {} and {})", groupId, tender.getGroupId());
                        throw new UnrecoverableException("Two different group IDs");
                    } else {
                        // assign all matched tenders to the same group
                        tender.setGroupId(groupId).setMatchedBy(getMatchedBy());
                        matchedTenderDao.save(tender);
                    }
                }
            }

            matchingResult.setGroupId(groupId);
            matchingResult.setMatchedBy(getMatchedBy());
            matchingResult.setMatched(true);
        }

        return matchingResult;
    }
}
