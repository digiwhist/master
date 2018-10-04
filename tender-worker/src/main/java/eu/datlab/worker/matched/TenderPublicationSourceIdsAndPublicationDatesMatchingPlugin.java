package eu.datlab.worker.matched;

import eu.dl.dataaccess.dao.MatchedTenderDAO;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.worker.utils.ArrayUtils;
import java.time.LocalDate;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * This plugin matches tenders with non-empty intersection of publications source ids and publication dates.
 */
public class TenderPublicationSourceIdsAndPublicationDatesMatchingPlugin extends BaseTenderMatchingPlugin {

    private static final String MATCHED_BY = "tenderPublicationSourceIdsAndPublicationDate";

    /**
     * @see BaseTenderMatchingPlugin#BaseTenderMatchingPlugin(
     * eu.dl.dataaccess.dao.MatchedTenderDAO, boolean)
     *
     * @param dao
     *      matched tender dao
     * @param isStrict
     *      enable/disable strict mode
     */
    public TenderPublicationSourceIdsAndPublicationDatesMatchingPlugin(final MatchedTenderDAO dao,
        final boolean isStrict) {
        super(dao, isStrict);
    }

    /**
     * @see BaseTenderMatchingPlugin#BaseTenderMatchingPlugin(
     * eu.dl.dataaccess.dao.MatchedTenderDAO)
     *
     * @param dao
     *      matched tender dao
     */
    public TenderPublicationSourceIdsAndPublicationDatesMatchingPlugin(final MatchedTenderDAO dao) {
        super(dao);
    }

    @Override
    public final String getMatchedBy() {
        return MATCHED_BY;
    }

    @Override
    public final List<MatchedTender> getMatchedTenders(final MatchedTender matchedTender) {
        if (matchedTender == null || matchedTender.getPublications() == null) {
            return Collections.emptyList();
        }

        Map<String, LocalDate> publications = new HashMap<>();

        matchedTender.getPublications().stream()
            .filter(n ->  n.getSourceId() != null && n.getPublicationDate() != null)
            .filter(ArrayUtils.distinct(n -> n.getSourceId() + "|" + n.getPublicationDate()))
            .forEach(n -> publications.put(n.getSourceId(), n.getPublicationDate()));
            
        return matchedTenderDao.getByPublicationSourceIdsAndPublicationDates(publications);
    }
}
