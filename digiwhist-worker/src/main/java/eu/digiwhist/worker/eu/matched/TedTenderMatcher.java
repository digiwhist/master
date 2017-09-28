package eu.digiwhist.worker.eu.matched;

import eu.digiwhist.worker.matched.BaseDigiwhistTenderMatcher;
import eu.digiwhist.worker.matched.TenderPublicationSourceIdsMatchingPlugin;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import static eu.dl.dataaccess.utils.DigestUtils.bodyHash;
import java.io.UnsupportedEncodingException;
import java.util.List;
import java.util.UUID;
import org.apache.commons.codec.digest.DigestUtils;

/**
 * Matcher for TED.
 *
 */
public class TedTenderMatcher extends BaseDigiwhistTenderMatcher {

    private static final String VERSION = "1.0";

    private static final String TED_TENDER_PLUGIN = "tedTender";

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final void registerBodyPlugins() {
    }

    @Override
    protected final void registerTenderPlugins() {
        tenderPluginRegistry.registerPlugin(TED_TENDER_PLUGIN,
            new TenderPublicationSourceIdsMatchingPlugin(matchedTenderDao));
    }

    @Override
    protected final String generateBodyHash(final MatchedBody matchedBody) {
        return bodyHash(matchedBody);
    }

    @Override
    protected final String generateTenderHash(final MatchedTender matchedTender) {
        String publication = "";

        try {
            List<Publication> cleanPublications = matchedTender.getPublications();
            if (cleanPublications != null) {
                for (Publication cleanPublication : cleanPublications) {
                    if (cleanPublication.getIsIncluded()) {
                        publication = cleanPublication.getSourceId();
                    }
                }
            }

            if (publication == null || publication.isEmpty()) {
                // there is no source id assigned, the hash cannot be reasonably
                // calculated
                publication = UUID.randomUUID().toString();
            }

            byte[] data = publication.getBytes("UTF-8");
            return DigestUtils.sha1Hex(data);
        } catch (UnsupportedEncodingException e) {
            logger.error("Unable to convert \"{}\" to UTF-8", publication);
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
