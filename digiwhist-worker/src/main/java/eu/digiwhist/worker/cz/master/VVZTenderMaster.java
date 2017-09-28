package eu.digiwhist.worker.cz.master;

import java.util.Currency;
import java.util.List;

import eu.digiwhist.worker.master.BaseDigiwhistTenderMaster;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.matched.MatchedTender;

/**
 * Masters tender data for VVZ (new Vestnik).
 */
public class VVZTenderMaster extends BaseDigiwhistTenderMaster {

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
        return items;
    }

    @Override
    protected final Currency getNationalCurrency() {
        return Currency.getInstance("CZK");
    }

    @Override
    protected final MasterTender sourceSpecificPostprocessData(final MasterTender item) {
        return item;
    }
}
