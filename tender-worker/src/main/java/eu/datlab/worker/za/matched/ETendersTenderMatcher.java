package eu.datlab.worker.za.matched;

import eu.datlab.worker.matched.BaseDatlabTenderMatcher;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.dataaccess.utils.TenderUtils;

import java.util.UUID;

import static org.apache.commons.codec.digest.DigestUtils.sha256Hex;

/**
 * ETenders matcher.
 */
public class ETendersTenderMatcher extends BaseDatlabTenderMatcher {
    private static final String VERSION = "1.0";

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final void registerBodyPlugins() {
    }

    @Override
    protected final void registerTenderPlugins() {
    }

    @Override
    protected final String generateBodyHash(final MatchedBody body) {
        if (body == null) {
            return null;
        }

        String name = null;
        if (body.getName() != null) {
            name = body.getName();
        }

        if (name == null || name.isEmpty()) {
            name = UUID.randomUUID().toString();
        }

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
