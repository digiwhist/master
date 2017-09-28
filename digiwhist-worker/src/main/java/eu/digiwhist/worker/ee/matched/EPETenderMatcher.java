package eu.digiwhist.worker.ee.matched;

import eu.digiwhist.worker.matched.BaseDigiwhistTenderMatcher;
import eu.digiwhist.worker.matched.TenderPublicationHumanReadableUrlsMatchingPlugin;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import static eu.dl.dataaccess.utils.DigestUtils.bodyHash;
import eu.dl.dataaccess.utils.TenderUtils;

/**
 * Tender matcher for E-procurement in Estonia.
 *
 * @author Tomas Mrazek
 */
public class EPETenderMatcher extends BaseDigiwhistTenderMatcher {
    private static final String VERSION = "1.0";

    protected static final String TENDER_PLUGIN = "epe";

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final void registerBodyPlugins() {
    }

    @Override
    protected final void registerTenderPlugins() {
        tenderPluginRegistry.registerPlugin(TENDER_PLUGIN,
            new TenderPublicationHumanReadableUrlsMatchingPlugin(matchedTenderDao));
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
