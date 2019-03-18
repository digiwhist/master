package eu.datlab.worker.cz.master;

import eu.datlab.worker.master.BaseDatlabTenderMaster;
import eu.datlab.worker.master.MasterUtils;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.worker.master.plugin.generic.LogicalORPlugin;

import java.util.Arrays;
import java.util.List;

/**
 * Masters tender data for VVZ (new Vestnik).
 */
public class VVZTenderMaster extends BaseDatlabTenderMaster {

    private static final String VERSION = "1.0";

    @Override
    protected final void registerSpecificPlugins() {
        logger.debug("No specific plugins to be registered.");
    }

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final String getIncomingQueueName() {
        return getIncomingQueueNameFromConfig();
    }

    @Override
    protected final List<MatchedTender> sourceSpecificPreprocessData(final List<MatchedTender> items) {
        //exclude all except first published CONTRACT_AWARD from mastering if maste tender will be a FA
        // because the
        MasterTender tender = new MasterTender();

        LogicalORPlugin plugin = new LogicalORPlugin(Arrays.asList("isFrameworkAgreement"));
        tender = (MasterTender) plugin.master(items, tender, items);

        List<MatchedTender> reducedList = MasterUtils.reduceContractAwards(items, tender);

        return reducedList;
    }

    @Override
    protected final MasterTender sourceSpecificPostprocessData(final MasterTender item) {
        return item;
    }
}
