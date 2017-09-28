package eu.digiwhist.worker.hu.matched;

import eu.digiwhist.dataaccess.dto.codetables.PublicationSources;
import eu.digiwhist.worker.matched.BaseDigiwhistTenderMatcher;
import eu.digiwhist.worker.matched.TenderPublicationSourceIdsMatchingPlugin;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import org.apache.commons.codec.digest.DigestUtils;

import java.io.UnsupportedEncodingException;
import java.util.UUID;

import static eu.dl.dataaccess.utils.DigestUtils.bodyHash;

/**
 * Hungary matching.
 */
public class KHTenderMatcher extends BaseDigiwhistTenderMatcher {
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
        tenderPluginRegistry
                .registerPlugin("TenderPublicationSourceIdsMatchingPlugin",
                        new TenderPublicationSourceIdsMatchingPlugin(matchedTenderDao, false));
    }

    @Override
    protected final String generateBodyHash(final MatchedBody matchedBody) {
        return bodyHash(matchedBody);
    }

    @Override
    protected final String generateTenderHash(final MatchedTender matchedTender) {
        String hash = null;

        try {
            // if there is reference on previous publication, generate hash from its id
            for (Publication publication : matchedTender.getPublications()) {
                if (!publication.getIsIncluded() && publication.getSourceId() != null && !publication.getSourceId()
                        .isEmpty() && publication.getSource()
                        .toString().equals(PublicationSources.HU_KH)) {
                    hash = publication.getSourceId();
                }
            }

            // if there is no previous publication (hash is still null) use own id to generate hash
            if (hash == null) {
                hash = matchedTender.getPublications().get(0).getSourceId();
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
        return this.getClass().getName() + TENDER_MESSAGING_TAG_SUFFIX;
    }

    @Override
    protected final String getBodyMessagingTag() {
        return this.getClass().getName() + BODY_MESSAGING_TAG_SUFFIX;
    }
}
