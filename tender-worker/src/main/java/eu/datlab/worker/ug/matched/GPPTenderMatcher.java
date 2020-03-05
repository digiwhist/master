package eu.datlab.worker.ug.matched;

/**
 * Tender matcher for Uganda.
 *
 * @author Tomas Mrazek
 */
public class GPPTenderMatcher extends BaseGPPTenderMatcher {
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
    }
}
