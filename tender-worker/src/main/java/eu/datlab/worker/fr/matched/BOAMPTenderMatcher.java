package eu.datlab.worker.fr.matched;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.datlab.worker.matched.BaseDatlabTenderMatcher;
import eu.datlab.worker.matched.TenderPublicationSourceIdsMatchingPlugin;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.generic.Publication;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import static eu.dl.dataaccess.utils.DigestUtils.bodyHash;
import java.io.UnsupportedEncodingException;
import java.util.UUID;
import org.apache.commons.codec.digest.DigestUtils;

/**
 * Tender matcher for France.
 *
 * @author Marek Mikes
 */
public class BOAMPTenderMatcher extends BaseDatlabTenderMatcher {
    private static final String VERSION = "1";

    private static final String BOAMP_TENDER_PLUGIN = "boamp";

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final void registerBodyPlugins() {
    }

    @Override
    protected final void registerTenderPlugins() {
        tenderPluginRegistry.registerPlugin(BOAMP_TENDER_PLUGIN,
            // the plugin joins different groups (created from FTP data) when we get publication from web
            new TenderPublicationSourceIdsMatchingPlugin(matchedTenderDao, false));
    }

    @Override
    protected final String generateBodyHash(final MatchedBody matchedBody) {
        return bodyHash(matchedBody);
    }

    @Override
    protected final String generateTenderHash(final MatchedTender matchedTender) {
        String stringToHash = "";

        try {
            // Use the publication source ID of the oldest publication from BOAMP source to generate hash and detect
            // proper tender.
            // At least web publication should match with some publication by hash.
            Publication publication = null;
            for (Publication matchedPublication : matchedTender.getPublications()) {
                if (matchedPublication.getSource() != null
                        && (matchedPublication.getSource().toString().equals(PublicationSources.FR_BOAMP_WEB)
                            || matchedPublication.getSource().toString().equals(PublicationSources.FR_BOAMP_FTP))
                        && matchedPublication.getSourceId() != null
                        && !matchedPublication.getSourceId().isEmpty()
                        && matchedPublication.getPublicationDate() != null
                        && (publication == null
                            || publication.getPublicationDate().isAfter(matchedPublication.getPublicationDate()))) {
                    publication = matchedPublication;
                }
            }

            if (publication != null) {
                stringToHash = publication.getSourceId();
            } else {
                // there is no publication assigned, the hash cannot be reasonably calculated
                stringToHash = UUID.randomUUID().toString();
            }

            byte[] data = stringToHash.getBytes("UTF-8");
            return DigestUtils.sha1Hex(data);
        } catch (UnsupportedEncodingException e) {
            logger.error("Unable to convert \"{}\" to UTF-8", stringToHash);
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
