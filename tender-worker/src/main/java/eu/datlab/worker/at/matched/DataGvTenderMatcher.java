package eu.datlab.worker.at.matched;

import eu.datlab.worker.matched.BaseDatlabTenderMatcher;
import eu.datlab.worker.matched.TenderPublicationMachineReadableUrlsMatchingPlugin;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import org.apache.commons.codec.digest.DigestUtils;

import java.io.UnsupportedEncodingException;
import java.util.UUID;

import static eu.dl.dataaccess.utils.DigestUtils.bodyHash;

/**
 * Provisional matcher for data.gv.at tenders.
 *
 * @author Miroslav Brezik
 */
public class DataGvTenderMatcher extends BaseDatlabTenderMatcher {
    private static final String VERSION = "1.0";
    protected static final String DATA_GV_TENDER_PLUGIN = "data_gv";


    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final void registerTenderPlugins() {
        tenderPluginRegistry.registerPlugin(DATA_GV_TENDER_PLUGIN,
                new TenderPublicationMachineReadableUrlsMatchingPlugin(matchedTenderDao, false));
    }

    @Override
    protected void registerBodyPlugins() {

    }

    @Override
    protected final String generateBodyHash(final MatchedBody matchedBody) { return bodyHash(matchedBody); }

    @Override
    protected final String generateTenderHash(final MatchedTender matchedTender) {
        try {
            byte[] data = UUID.randomUUID().toString().getBytes("UTF-8");
            return DigestUtils.sha1Hex(data);
        } catch (UnsupportedEncodingException e) {
            logger.error("Unable to convert random ID to UTF-8");
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
