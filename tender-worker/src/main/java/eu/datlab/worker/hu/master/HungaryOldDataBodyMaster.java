package eu.datlab.worker.hu.master;

import eu.datlab.worker.master.BaseDatlabBodyMaster;
import eu.dl.dataaccess.dto.master.MasterBody;
import eu.dl.dataaccess.dto.matched.MatchedBody;

import java.util.List;

/**
 * Masters body data for old hungary data.
 */
public class HungaryOldDataBodyMaster extends BaseDatlabBodyMaster {

    private static final String VERSION = "1.0";

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final String getIncomingQueueName() {
        return getIncomingQueueNameFromConfig();
    }

    @Override
    protected final void registerSpecificPlugins() {
    }

    @Override
    protected final List<MatchedBody> sourceSpecificPreprocessData(final List<MatchedBody> items) {
        return items;
    }

    @Override
    protected final MasterBody sourceSpecificPostprocessData(final MasterBody item) {
        return item;
    }
}