package eu.datlab.worker.es.master;

import eu.datlab.worker.master.BaseDatlabBodyMaster;
import eu.dl.dataaccess.dto.master.MasterBody;
import eu.dl.dataaccess.dto.matched.MatchedBody;

import java.util.List;

/**
 * Cleans Spanish bodies from Hacienda source.
 */
public class HaciendaBodyMaster extends BaseDatlabBodyMaster {
    private static final String VERSION = "1.0";

    @Override
    protected void registerSpecificPlugins() {

    }

    @Override
    protected final List<MatchedBody> sourceSpecificPreprocessData(final List<MatchedBody> items) {
        return items;
    }

    @Override
    protected final MasterBody sourceSpecificPostprocessData(final MasterBody item) {
        return item;
    }

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final String getIncomingQueueName() {
        return getIncomingQueueNameFromConfig();
    }
}
