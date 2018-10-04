package eu.datlab.worker.cz.master;

import eu.datlab.worker.master.BaseDatlabTenderMaster;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.matched.MatchedTender;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Masters tender data for Vestnik.
 */
public class VestnikTenderMaster extends BaseDatlabTenderMaster {

    private static final String VERSION = "1.1";

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
        List preprocessedData = items.stream()
                .filter(isNotContractAmendment())
                .collect(Collectors.toList());
        return preprocessedData;
    }

    @Override
    protected final MasterTender sourceSpecificPostprocessData(final MasterTender item) {
        return item;
    }
}
