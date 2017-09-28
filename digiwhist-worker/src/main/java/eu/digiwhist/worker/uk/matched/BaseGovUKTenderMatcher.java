package eu.digiwhist.worker.uk.matched;

import eu.digiwhist.worker.matched.BaseDigiwhistTenderMatcher;
import eu.digiwhist.worker.matched.TenderPublicationSourceIdsMatchingPlugin;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.dataaccess.utils.DigestUtils;
import eu.dl.dataaccess.utils.TenderUtils;

/**
 * Tender matcher for GOV UK.
 */
public class BaseGovUKTenderMatcher extends BaseDigiwhistTenderMatcher {
    private static final String VERSION = "1.0";

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final void registerTenderPlugins() {
        tenderPluginRegistry.registerPlugin("SourceIdsMatchingPlugin",
                new TenderPublicationSourceIdsMatchingPlugin(matchedTenderDao));
    }

    @Override
    protected final void registerBodyPlugins() {

    }

    @Override
    protected final String generateBodyHash(final MatchedBody matchedBody) {
        return DigestUtils.bodyHash(matchedBody);
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
