package eu.datlab.worker.it.matched;

import eu.datlab.worker.matched.BaseDatlabTenderMatcher;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.matched.MatchedTender;

import java.io.UnsupportedEncodingException;
import java.util.List;
import java.util.UUID;

import static eu.dl.dataaccess.utils.DigestUtils.bodyHash;

/**
 * Matcher for IT tenders.
 */
public class ANACTenderMatcher extends BaseDatlabTenderMatcher {

    private static final String VERSION = "1.0";

    private static final String TENDER_PLUGIN = "anacTender";

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final void registerBodyPlugins() {
        bodyPluginRegistry.unRegisterPlugin(EXACT_MATCH_ETALON_PLUGIN);
        bodyPluginRegistry.unRegisterPlugin(APPROXIMATE_MATCH_ETALON_PLUGIN);
        bodyPluginRegistry.unRegisterPlugin(EXACT_MATCH_BODY_PLUGIN);
        bodyPluginRegistry.unRegisterPlugin(APPROXIMATE_MATCH_BODY_PLUGIN);
    }

    @Override
    protected final void registerTenderPlugins() {
//        tenderPluginRegistry.registerPlugin(TENDER_PLUGIN,
//                new TenderPublicationSourceIdsMatchingPlugin(matchedTenderDao));
    }

    @Override
    protected final String generateBodyHash(final MatchedBody matchedBody) {
        return bodyHash(matchedBody);
    }

    @Override
    protected final String generateTenderHash(final MatchedTender matchedTender) {
//        return TenderUtils.generateTenderHash(matchedTender);
        if (matchedTender == null) {
            return null;
        }

        String sourceTenderId = "";
        String url = "";
        String hashBase = "";

        try {
            List<Publication> cleanPublications = matchedTender.getPublications();
            for (Publication cleanPublication : cleanPublications) {
                if (cleanPublication.getIsIncluded()) {
                    sourceTenderId = cleanPublication.getSourceTenderId();
                    if (cleanPublication.getMachineReadableUrl() != null) {
                        url = cleanPublication.getMachineReadableUrl().getHost();
                    }
                    break;
                }
            }

            if (sourceTenderId == null || sourceTenderId.isEmpty() || url == null || url.isEmpty()) {
                // there is no source id assigned, the hash cannot be reasonably calculated
                hashBase = UUID.randomUUID().toString();
            } else {
                hashBase = url + sourceTenderId;
            }

            byte[] data = hashBase.getBytes("UTF-8");
            return org.apache.commons.codec.digest.DigestUtils.sha1Hex(data);
        } catch (UnsupportedEncodingException e) {
            logger.error("Unable to convert \"{}\" to UTF-8", sourceTenderId);
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
