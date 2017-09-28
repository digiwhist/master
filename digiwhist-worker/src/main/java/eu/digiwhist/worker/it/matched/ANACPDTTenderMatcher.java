package eu.digiwhist.worker.it.matched;

import eu.digiwhist.worker.matched.BaseDigiwhistTenderMatcher;
import eu.digiwhist.worker.matched.TenderPublicationSourceIdsMatchingPlugin;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import static eu.dl.dataaccess.utils.DigestUtils.bodyHash;
import java.io.UnsupportedEncodingException;
import java.util.UUID;
import org.apache.commons.codec.digest.DigestUtils;

/**
 * Matcher for "Portale della trasparenza" tenders.
 */
public class ANACPDTTenderMatcher extends BaseDigiwhistTenderMatcher {

    private static final String VERSION = "1.0";

    private static final String TENDER_PLUGIN = "anacpdtTender";

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final void registerBodyPlugins() {        
    }

    @Override
    protected final void registerTenderPlugins() {
        tenderPluginRegistry.registerPlugin(TENDER_PLUGIN,
            new TenderPublicationSourceIdsMatchingPlugin(matchedTenderDao));
    }

    @Override
    protected final String generateBodyHash(final MatchedBody matchedBody) {
        return bodyHash(matchedBody);
    }

    @Override
    protected final String generateTenderHash(final MatchedTender matchedTender) {
        String hash = "";

        try {
            Publication includedPublications = matchedTender.getPublications().stream()
                .filter(p -> p.getIsIncluded() != null && p.getIsIncluded())
                .findFirst()
                .orElse(null);

            if (includedPublications != null) {
                hash = includedPublications.getSourceId();
            }
            
            //there is no data for hash, the hash cannot be reasonably calculated
            if (hash == null || hash.isEmpty()) {                
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
