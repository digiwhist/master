package eu.digiwhist.worker.ie.matched;

import eu.digiwhist.worker.matched.BaseDigiwhistTenderMatcher;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.dataaccess.utils.TenderUtils;

import static eu.dl.dataaccess.utils.DigestUtils.bodyHash;

/**
 * Tender matched for Ireland ETenders.
 *
 * @author Marek Mikes
 */
public class ETendersTenderMatcher extends BaseDigiwhistTenderMatcher {
    private static final String VERSION = "1";

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final void registerBodyPlugins() {
    }

    @Override
    protected final void registerTenderPlugins() {
        // no other matching plugins needed, because source tender id (used for hash) is never missing
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
}
