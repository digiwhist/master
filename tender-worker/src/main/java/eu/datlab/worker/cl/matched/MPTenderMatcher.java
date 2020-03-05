package eu.datlab.worker.cl.matched;

import eu.datlab.worker.matched.BaseDatlabTenderMatcher;
import eu.datlab.worker.matched.TenderPublicationSourceIdsAndPublicationDatesMatchingPlugin;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.dataaccess.utils.TenderUtils;

import static eu.dl.dataaccess.utils.DigestUtils.bodyHash;

/**
 * Tender matcher for France.
 *
 * @author Michal Riha
 */
public class MPTenderMatcher extends BaseDatlabTenderMatcher {
    private static final String VERSION = "1";

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final void registerBodyPlugins() {
        bodyPluginRegistry.unRegisterPlugin(EXACT_MATCH_ETALON_PLUGIN);
        bodyPluginRegistry.unRegisterPlugin(APPROXIMATE_MATCH_ETALON_PLUGIN);
    }

    @Override
    protected final void registerTenderPlugins() {
        tenderPluginRegistry.registerPlugin("mpTender",
            new TenderPublicationSourceIdsAndPublicationDatesMatchingPlugin(matchedTenderDao, false));
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
