package eu.datlab.worker.matched;

import eu.dl.dataaccess.dao.MatchedTenderDAO;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.matched.MatchedTender;

import java.util.List;

/**
 * This plugin matches tenders with non-empty intersection of publications source ids.
 */
public class TenderPublicationSourceIdsMatchingPlugin extends BaseTenderMatchingPlugin {

    private static final String MATCHED_BY = "tenderPublicationSourceIds";

    private List<String> sourceIds;

    /**
     * @see BaseTenderMatchingPlugin#BaseTenderMatchingPlugin(
     * eu.dl.dataaccess.dao.MatchedTenderDAO, boolean)
     *
     * @param dao
     *      matched tender dao
     * @param isStrict
     *      enable/disable strict mode
     */
    public TenderPublicationSourceIdsMatchingPlugin(final MatchedTenderDAO dao, final boolean isStrict) {
        super(dao, isStrict);
    }

    /**
     * @see BaseTenderMatchingPlugin#BaseTenderMatchingPlugin(
     * eu.dl.dataaccess.dao.MatchedTenderDAO)
     *
     * @param dao
     *      matched tender dao
     */
    public TenderPublicationSourceIdsMatchingPlugin(final MatchedTenderDAO dao) {
        super(dao);
    }

    @Override
    public final String getMatchedBy() {
        return MATCHED_BY;
    }

    @Override
    public final List<MatchedTender> getMatchedTenders(final MatchedTender matchedTender) {
        return matchedTenderDao.getByPublicationSourceIds(getSourceIds(matchedTender));
    }

    @Override
    public final boolean isMatchable(final MatchedTender item) {
        return !getSourceIds(item).isEmpty();
    }

    /**
     * @param t
     *      matched tender
     * @return list of source ids of all publications or empty list
     */
    private List<String> getSourceIds(final MatchedTender t) {
        return MatchingPluginUtils.getPublicationsValues(t, Publication::getSourceId);
    }
}
