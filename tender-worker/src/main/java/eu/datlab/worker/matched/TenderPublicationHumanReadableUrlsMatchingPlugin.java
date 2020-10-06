package eu.datlab.worker.matched;

import eu.dl.dataaccess.dao.MatchedTenderDAO;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import java.net.URL;
import java.util.List;

/**
 * This plugin matches tenders with non-empty intersection of publications human readable URLs.
 */
public class TenderPublicationHumanReadableUrlsMatchingPlugin extends BaseTenderMatchingPlugin {

    private static final String MATCHED_BY = "tenderPublicationHumanReadableUrls";

    /**
     * @see BaseTenderMatchingPlugin#BaseTenderMatchingPlugin(
     * eu.dl.dataaccess.dao.MatchedTenderDAO, boolean)
     * 
     * @param dao
     *      matched tender dao
     * @param isStrict
     *      enable/disable strict mode
     */
    public TenderPublicationHumanReadableUrlsMatchingPlugin(final MatchedTenderDAO dao, final boolean isStrict) {
        super(dao, isStrict);
    }

    /**
     * @see BaseTenderMatchingPlugin#BaseTenderMatchingPlugin(
     * eu.dl.dataaccess.dao.MatchedTenderDAO)
     *
     * @param dao
     *      matched tender dao
     */
    public TenderPublicationHumanReadableUrlsMatchingPlugin(final MatchedTenderDAO dao) {
        super(dao);
    }

    @Override
    public final String getMatchedBy() {
        return MATCHED_BY;
    }

    @Override
    public final List<MatchedTender> getMatchedTenders(final MatchedTender matchedTender) {
        return matchedTenderDao.getByPublicationHumanReadableUrls(getHumanReadableUrls(matchedTender));
    }

    @Override
    public final boolean isMatchable(final MatchedTender item) {
        return !getHumanReadableUrls(item).isEmpty();
    }

    /**
     * @param t
     *      matched tender
     * @return list of human readable urls of all publications or empty list
     */
    private List<URL> getHumanReadableUrls(final MatchedTender t) {
        return MatchingPluginUtils.getPublicationsValues(t, Publication::getHumanReadableUrl);
    }
}
