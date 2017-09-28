package eu.digiwhist.worker.lt.master;

import eu.digiwhist.worker.master.BaseDigiwhistTenderMaster;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.matched.MatchedTender;

import java.util.Currency;
import java.util.List;

/**
 * Created by michalriha on 27/06/2017.
 */
public class CVPISTenderMaster extends BaseDigiwhistTenderMaster {
    private static final String VERSION = "1";

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
    protected final List<MatchedTender> sourceSpecificPreprocessData(final List<MatchedTender> items) {
        return items;
    }

    @Override
    protected final Currency getNationalCurrency() {
        return Currency.getInstance("LT");
    }

    @Override
    protected final MasterTender sourceSpecificPostprocessData(final MasterTender item) {
        return item;
    }
}