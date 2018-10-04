package eu.datlab.worker.sk.matched;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.datlab.worker.matched.BaseDatlabTenderMatcher;
import eu.datlab.worker.matched.TenderPublicationHumanReadableUrlsMatchingPlugin;
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
 * Matcher for UVO.
 *
 */
public class UvoTenderMatcher extends BaseDatlabTenderMatcher {

    private static final String VERSION = "1.0";

    protected static final String UVO_MATCH_BODY_PLUGIN = "uvoMatch";
    protected static final String UVO_CREATE_BODY_PLUGIN = "uvoCreate";
    protected static final String UVO_TENDER_PLUGIN = "uvo";

    private static final String SOURCE = PublicationSources.SK_UVO;

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final void registerBodyPlugins() {
    }

    @Override
    protected final void registerTenderPlugins() {
        tenderPluginRegistry.registerPlugin(UVO_TENDER_PLUGIN,
            new TenderPublicationHumanReadableUrlsMatchingPlugin(matchedTenderDao));
    }

    @Override
    protected final String generateBodyHash(final MatchedBody matchedBody) {
        return bodyHash(matchedBody);
    }

    @Override
    protected final String generateTenderHash(final MatchedTender matchedTender) {
        String publication = "";

        try {
            List<Publication> matchedPublications = matchedTender.getPublications();

            // Use the URL of the oldest publication from UVO source to generate hash and detect proper tender.
            // Because we don't have publication date for publications isIncluded = false we search for the shortest
            // publication. When more publications have the same string length than the lexicographically smallest
            // is the oldest.
            for (Publication matchedPublication : matchedPublications) {
                if (matchedPublication.getSource() != null && matchedPublication.getSource().toString().equals(SOURCE)
                        && matchedPublication.getHumanReadableUrl() != null
                        && (
                        publication.isEmpty()
                        || publication.length() > matchedPublication.getHumanReadableUrl().toString().length()
                        || publication.compareTo(matchedPublication.getHumanReadableUrl().toString()) > 0)) {
                    publication = matchedPublication.getHumanReadableUrl().toString();
                }
            }

            if (publication.isEmpty()) {
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
        return this.getName() + TENDER_MESSAGING_TAG_SUFFIX;
    }

    @Override
    protected final String getBodyMessagingTag() {
        return this.getName() + BODY_MESSAGING_TAG_SUFFIX;
    }
}
