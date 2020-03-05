package eu.datlab.worker.id.matched;

import eu.datlab.worker.matched.BaseDatlabTenderMatcher;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.dataaccess.utils.DigestUtils;
import java.io.UnsupportedEncodingException;
import java.util.List;
import java.util.UUID;

/**
 * Tender matched for Indonesia.
 *
 * @author Tomas Mrazek
 */
public class LPSETenderMatcher extends BaseDatlabTenderMatcher {
    private static final String VERSION = "1.0";

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final void registerBodyPlugins() {
        bodyPluginRegistry.unRegisterPlugin(EXACT_MATCH_BODY_PLUGIN);
        bodyPluginRegistry.unRegisterPlugin(EXACT_MATCH_ETALON_PLUGIN);
        bodyPluginRegistry.unRegisterPlugin(APPROXIMATE_MATCH_BODY_PLUGIN);
        bodyPluginRegistry.unRegisterPlugin(APPROXIMATE_MATCH_ETALON_PLUGIN);
    }

    @Override
    protected final void registerTenderPlugins() {
    }

    @Override
    protected final String generateBodyHash(final MatchedBody matchedBody) {
        return DigestUtils.bodyHash(matchedBody);
    }

    @Override
    protected final String generateTenderHash(final MatchedTender matchedTender) {
        if (matchedTender == null) {
            return null;
        }

        String publication = "";

        try {
            List<Publication> cleanPublications = matchedTender.getPublications();
            for (Publication cleanPublication : cleanPublications) {
                if (cleanPublication.getIsIncluded()) {
                    publication = cleanPublication.getSourceId();
                    break;
                }
            }

            if (publication == null || publication.isEmpty()) {
                // there is no source id assigned, the hash cannot be reasonably calculated
                publication = UUID.randomUUID().toString();
            }

            byte[] data = publication.getBytes("UTF-8");
            return org.apache.commons.codec.digest.DigestUtils.sha1Hex(data);
        } catch (UnsupportedEncodingException e) {
            logger.error("Unable to convert \"{}\" to UTF-8", publication);
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
