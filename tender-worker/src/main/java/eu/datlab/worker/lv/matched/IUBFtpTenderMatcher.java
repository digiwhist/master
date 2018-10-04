package eu.datlab.worker.lv.matched;

import eu.datlab.worker.matched.BaseDatlabTenderMatcher;
import eu.datlab.worker.matched.TenderPublicationSourceIdsMatchingPlugin;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import static eu.dl.dataaccess.utils.DigestUtils.bodyHash;
import java.io.UnsupportedEncodingException;
import java.util.List;
import java.util.UUID;
import org.apache.commons.codec.digest.DigestUtils;

/**
 * Matcher for IUB ftp tenders.
 */
public class IUBFtpTenderMatcher extends BaseDatlabTenderMatcher {

    private static final String VERSION = "1.0";

    private static final String TENDER_PLUGIN = "iubTender";

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
        String hash = null;

        try {            
            String assignedId = matchedTender.getBuyerAssignedId();

            // attempt to find buyer ORGANIZATION_ID (always only one buyer with one ORAGANIZATION_ID)
            String buyerId = null;
            List<MatchedBody> buyers = matchedTender.getBuyers();
            if (buyers != null && !buyers.isEmpty()) {
                MatchedBody buyer = matchedBodyDao.getById(buyers.get(0).getId());
                
                if (buyer != null && buyer.getBodyIds() != null) {
                    buyerId = buyer.getBodyIds().stream()
                        .filter(b -> b.getType().equals(BodyIdentifier.Type.ORGANIZATION_ID))                        
                        .findFirst()
                        .map(b -> b.getId())
                        .orElse(null);                    
                }
            }

            if (assignedId != null && !assignedId.isEmpty() && buyerId != null && !buyerId.isEmpty()) {
                hash = assignedId + "|" + buyerId;
            } else {
                // there is no source id assigned, the hash cannot be reasonably
                // calculated
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
