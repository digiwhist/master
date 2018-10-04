package eu.datlab.worker.matched;

import eu.dl.dataaccess.dao.MatchedTenderDAO;
import eu.dl.dataaccess.dto.matched.MatchedTender;

import java.util.Collections;
import java.util.List;

/**
 * This plugin matches tenders with the same documents URL.
 */
public class TenderDocumentsUrlMatchingPlugin extends BaseTenderMatchingPlugin {

    private static final String MATCHED_BY = "tenderDocumentsLocation";

    /**
     * @see BaseTenderMatchingPlugin#BaseTenderMatchingPlugin(
     * eu.dl.dataaccess.dao.MatchedTenderDAO, boolean)
     *
     * @param dao
     *      matched tender dao
     * @param isStrict
     *      enable/disable strict mode
     */
    public TenderDocumentsUrlMatchingPlugin(final MatchedTenderDAO dao, final boolean isStrict) {
        super(dao, isStrict);
    }

    /**
     * @see BaseTenderMatchingPlugin#BaseTenderMatchingPlugin(
     * eu.dl.dataaccess.dao.MatchedTenderDAO)
     *
     * @param dao
     *      matched tender dao
     */
    public TenderDocumentsUrlMatchingPlugin(final MatchedTenderDAO dao) {
        super(dao);
    }

    @Override
    public final String getMatchedBy() {
        return MATCHED_BY;
    }

    @Override
    public final List<MatchedTender> getMatchedTenders(final MatchedTender matchedTender) {
        return matchedTender.getDocumentsLocation() == null
                ? Collections.emptyList()
                : matchedTenderDao.getByDocumentsUrl(matchedTender.getDocumentsLocation().getUrl());
    }
}
