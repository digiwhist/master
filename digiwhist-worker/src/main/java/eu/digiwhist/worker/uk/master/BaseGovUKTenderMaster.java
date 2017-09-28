package eu.digiwhist.worker.uk.master;

import java.util.Comparator;
import java.util.Currency;
import java.util.List;
import java.util.stream.Collectors;

import eu.digiwhist.worker.master.BaseDigiwhistTenderMaster;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.worker.utils.ArrayUtils;

/**
 * Tender master for GOV UK.
 */
public class BaseGovUKTenderMaster extends BaseDigiwhistTenderMaster {
    private static final String VERSION = "1.0";

    @Override
    protected final String getVersion() {
        return null;
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
        // Deduplicate
        return items.stream()
                .sorted(sortNewToOld())
                .filter(t -> t.getPublications() != null)
                .filter(ArrayUtils.distinct(t -> t.getPublications().get(0).getSourceId()))
                .collect(Collectors.toList());
    }

    /**
     * Comparator sorting matched tenders from new to old.
     *
     * @return compare result
     */
    private Comparator<MatchedTender> sortNewToOld() {
        return (matchedTender1, matchedTender2) -> matchedTender2.getModified().compareTo(matchedTender1.getModified());
    }
    
    @Override
    protected final Currency getNationalCurrency() {
        return Currency.getInstance("GBP");
    }

    @Override
    protected final MasterTender sourceSpecificPostprocessData(final MasterTender item) {
        return item;
    }
}
