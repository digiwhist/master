package eu.datlab.worker.es.matched;

import eu.datlab.worker.matched.BaseDatlabTenderMatcher;
import eu.datlab.worker.matched.TenderPublicationMachineReadableUrlsMatchingPlugin;
import eu.datlab.worker.matched.TenderSimilarityMatchingPlugin;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import static eu.dl.dataaccess.utils.DigestUtils.bodyHash;

import eu.dl.worker.utils.matched.MatchedUtils;

/**
 * Matcher for ES tenders.
 */
public class PCETenderMatcher extends BaseDatlabTenderMatcher {
    private static final String VERSION = "1.0";
    protected static final String PCE_TENDER_PLUGIN = "pce";
    protected static final String SIMILARITY_TENDER_PLUGIN = "similarity";


    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final void registerTenderPlugins() {
        tenderPluginRegistry.registerPlugin(PCE_TENDER_PLUGIN,
            new TenderPublicationMachineReadableUrlsMatchingPlugin(matchedTenderDao, false));

        tenderPluginRegistry.registerPlugin(SIMILARITY_TENDER_PLUGIN,
                new TenderSimilarityMatchingPlugin(matchedTenderDao, matchedBodyDao, this.getClass().getName(), false));
    }

    @Override
    protected void registerBodyPlugins() {

    }

    @Override
    protected final String generateBodyHash(final MatchedBody matchedBody) {
        return bodyHash(matchedBody);
    }

    @Override
    protected final String generateTenderHash(final MatchedTender matchedTender) {
        return MatchedUtils.generateRandomHash();
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
