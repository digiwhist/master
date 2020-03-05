package eu.datlab.worker.py.matched;

import eu.datlab.worker.matched.BaseDatlabTenderMatcher;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import org.apache.commons.codec.digest.DigestUtils;

import java.io.UnsupportedEncodingException;
import java.util.UUID;

import static eu.dl.dataaccess.utils.DigestUtils.bodyHash;

/**
 * Tender matcher for Paraguay.
 */
public class DNCPTenderMatcher extends BaseDatlabTenderMatcher {
    private static final String VERSION = "1.0";

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final void registerBodyPlugins() {
        bodyPluginRegistry.unRegisterPlugin(EXACT_MATCH_ETALON_PLUGIN);
        bodyPluginRegistry.unRegisterPlugin(APPROXIMATE_MATCH_ETALON_PLUGIN);

        bodyPluginRegistry.registerPlugin("dncp-buyer", new DNCPBuyersPlugin(matchedBodyDao));
    }

    @Override
    protected final void registerTenderPlugins() {
    }

    @Override
    protected final String generateBodyHash(final MatchedBody matchedBody) {
        return bodyHash(matchedBody);
    }

    @Override
    protected final String generateTenderHash(final MatchedTender matchedTender) {
        String hash = null;

        try {
            // attempts to find first included publication and from its sourceId parse indentifier used to matching.
            if (matchedTender.getPublications() != null) {
                hash = matchedTender.getPublications().stream()
                    .filter(n -> n.getIsIncluded()).findFirst()
                    .map(Publication::getSourceTenderId).orElse(null);
            }

            if (hash == null) {
                hash = UUID.randomUUID().toString();
            }

            byte[] data = hash.getBytes("UTF-8");
            return DigestUtils.sha1Hex(data);
        } catch (UnsupportedEncodingException e) {
            logger.error("Unable to convert \"{}\" to UTF-8", hash);
            throw new UnrecoverableException("Unable to convert data to UTF-8", e);
        }
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
