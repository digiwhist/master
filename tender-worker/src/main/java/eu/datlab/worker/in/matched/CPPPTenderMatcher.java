package eu.datlab.worker.in.matched;

import eu.datlab.worker.matched.BaseDatlabTenderMatcher;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.dataaccess.utils.TenderUtils;

import java.util.UUID;

import static org.apache.commons.codec.digest.DigestUtils.sha256Hex;

/**
 * Tender matcher for India.
 *
 * @author Tomas Mrazek
 */
public class CPPPTenderMatcher extends BaseDatlabTenderMatcher {
    private static final String VERSION = "1.0";

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
    }

    @Override
    protected final String generateBodyHash(final MatchedBody body) {
        if (body == null) {
            return null;
        }
        String name = body.getName() != null ? body.getName() : UUID.randomUUID().toString();
        return sha256Hex(name);
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
