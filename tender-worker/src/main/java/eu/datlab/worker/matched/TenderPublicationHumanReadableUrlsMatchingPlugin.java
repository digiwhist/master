package eu.datlab.worker.matched;

import eu.dl.dataaccess.dao.MatchedTenderDAO;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import java.net.URL;
import java.util.List;
import java.util.stream.Collectors;

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
        List<URL> publicationsSourceIds = matchedTender.getPublications().stream()
            .filter(p ->  p.getHumanReadableUrl() != null)
            .map(Publication::getHumanReadableUrl)
            .distinct()
            .collect(Collectors.toList());

        return matchedTenderDao.getByPublicationHumanReadableUrls(publicationsSourceIds);
    }
}
