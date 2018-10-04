package eu.datlab.worker.pl.matched;

import eu.datlab.worker.matched.BaseDatlabTenderMatcher;
import eu.datlab.worker.matched.TenderPublicationSourceIdsAndPublicationDatesMatchingPlugin;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import static eu.dl.dataaccess.utils.DigestUtils.bodyHash;
import java.io.UnsupportedEncodingException;
import java.util.UUID;
import org.apache.commons.codec.digest.DigestUtils;

/**
 * Matcher for UZP ftp tenders.
 */
public class UZPTenderMatcher extends BaseDatlabTenderMatcher {

    /**
     * Version public for other matchers.
     */
    public static final String VERSION = "1.0";

    private static final String TENDER_PLUGIN = "uzpTender";

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
            new TenderPublicationSourceIdsAndPublicationDatesMatchingPlugin(matchedTenderDao, false));
    }

    @Override
    protected final String generateBodyHash(final MatchedBody matchedBody) {
        return bodyHash(matchedBody);
    }

    @Override
    protected final String generateTenderHash(final MatchedTender matchedTender) {
        String hash = null;

        try {
            Publication includedPublication = matchedTender.getPublications().stream()
                .filter(n -> n.getIsIncluded() && n.getPublicationDate() != null)
                .findFirst().orElse(null);
            
            if (includedPublication != null && includedPublication.getSourceId() != null
                && includedPublication.getPublicationDate() != null) {
                hash = includedPublication.getSourceId() + "|" + includedPublication.getPublicationDate();
            } else {
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
