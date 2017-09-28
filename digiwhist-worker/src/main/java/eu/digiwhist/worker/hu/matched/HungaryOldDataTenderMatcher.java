package eu.digiwhist.worker.hu.matched;

import eu.digiwhist.worker.matched.BaseDigiwhistTenderMatcher;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.dataaccess.utils.TenderUtils;

import static eu.dl.dataaccess.utils.DigestUtils.bodyHash;

/**
 * Matcher for old hungary data.
 */
public class HungaryOldDataTenderMatcher extends BaseDigiwhistTenderMatcher {

    protected static final String VERSION = "1.0";

    /**
     * Constructor.
     */
    public HungaryOldDataTenderMatcher() {
    }

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected void registerBodyPlugins() {
    }

    @Override
    protected void registerTenderPlugins() {
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
        return this.getClass().getName() + TENDER_MESSAGING_TAG_SUFFIX;
    }

    @Override
    protected final String getBodyMessagingTag() {
        return this.getClass().getName() + BODY_MESSAGING_TAG_SUFFIX;
    }
}
