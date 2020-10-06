package eu.datlab.worker.ug.matched;

import eu.datlab.worker.matched.BaseDatlabTenderMatcher;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.dataaccess.utils.TenderUtils;

import java.util.UUID;

import static org.apache.commons.codec.digest.DigestUtils.sha256Hex;

/**
 * Basic tender matcher for Uganda.
 *
 * @author Tomas Mrazek
 */
public abstract class BaseGPPTenderMatcher extends BaseDatlabTenderMatcher {
    @Override
    protected final String generateBodyHash(final MatchedBody body) {
        if (body == null) {
            return null;
        }

        String name = body.getName();
        if (name == null || name.isEmpty()) {
            name = UUID.randomUUID().toString();
        }

        return sha256Hex(name);
    }

    @Override
    protected final String generateTenderHash(final MatchedTender matchedTender) {
        return TenderUtils.generateTenderHash(matchedTender);
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
