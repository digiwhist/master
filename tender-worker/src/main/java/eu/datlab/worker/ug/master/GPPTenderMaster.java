package eu.datlab.worker.ug.master;

import eu.datlab.worker.master.BaseDatlabTenderMaster;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.matched.MatchedTender;

import java.util.List;

/**
 * Tender master for Uganda.
 *
 * @author Tomas Mrazek
 */
public class GPPTenderMaster extends BaseDatlabTenderMaster {
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
        pluginRegistry.unRegisterPlugin("Lots");

        pluginRegistry.registerPlugin("LotsGPP", new GPPLotPlugin());
    }

    @Override
    protected final List<MatchedTender> sourceSpecificPreprocessData(final List<MatchedTender> items) {
        return items;
    }

    @Override
    protected final MasterTender sourceSpecificPostprocessData(final MasterTender item) {
        return item;
    }
}
