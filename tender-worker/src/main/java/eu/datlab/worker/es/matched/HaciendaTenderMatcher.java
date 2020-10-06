package eu.datlab.worker.es.matched;

import eu.datlab.worker.matched.BaseDatlabTenderMatcher;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.dataaccess.utils.TenderUtils;

import static eu.dl.dataaccess.utils.DigestUtils.bodyHash;
/**
 * Matches Spanish tenders from Hacienda source.
 */
public class HaciendaTenderMatcher extends BaseDatlabTenderMatcher {
    private static final String VERSION = "1.0";
    @Override
    protected final void registerTenderPlugins() {

    }

    @Override
    protected final void registerBodyPlugins() {

    }

    @Override
    protected final String generateBodyHash(final MatchedBody matchedBody) {
        return bodyHash(matchedBody);
    }

    @Override
    protected final String generateTenderHash(final MatchedTender matchedTender) {
        return TenderUtils.generateTenderHash(matchedTender);
    }

    @Override
    protected final String getTenderMessagingTag() {
        return this.getName() + TENDER_MESSAGING_TAG_SUFFIX;
    }

    @Override
    protected final String getBodyMessagingTag() {
        return this.getName() + BODY_MESSAGING_TAG_SUFFIX;
    }

    @Override
    protected final String getVersion() {
        return VERSION;
    }
}
