package eu.datlab.worker.es.clean;

import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.clean.plugin.CleaningPlugin;
import eu.dl.worker.clean.utils.CodeTableUtils;

import java.util.List;
import java.util.Map;

/**
 * Created by michal on 23.1.17.
 */
public class PCETenderSpecificPlugin implements CleaningPlugin<ParsedTender, CleanTender> {
    private Map<Enum, List<String>> deposits;

    /**
     * Default constructor.
     * @param deposits mapping of deposits
     */
    public PCETenderSpecificPlugin(final Map<Enum, List<String>> deposits) {
        this.deposits = deposits;
    }

    @Override
    public final CleanTender clean(final ParsedTender parsedItem, final CleanTender cleanItem) {
        final Enum deposit = CodeTableUtils.mapValue(parsedItem.getDeposits(), deposits);
        if (deposit != null) {
            cleanItem.setDeposits(deposit.toString());
        }

        return cleanItem;
    }
}
