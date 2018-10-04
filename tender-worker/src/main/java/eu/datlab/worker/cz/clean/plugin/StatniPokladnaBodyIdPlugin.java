package eu.datlab.worker.cz.clean.plugin;

import eu.datlab.dataaccess.dto.clean.CleanBudgetItem;
import eu.datlab.dataaccess.dto.parsed.ParsedBudgetItem;
import eu.dl.worker.clean.plugin.BaseCleaningPlugin;
import eu.dl.worker.utils.ArrayUtils;
import eu.dl.worker.utils.StringUtils;


/**
 * Statni pokladna budgets specific plugin for cleaning body identifiers, normalizes id to eight characters length.
 *
 * @author Tomas Mrazek
 */
public class StatniPokladnaBodyIdPlugin extends BaseCleaningPlugin<ParsedBudgetItem, CleanBudgetItem> {
    
    /**
     * Normalizes body identifiers to eight characters length. The plugin assumes that parsed record is already
     * cleaned so uses clean budget as source of data for body ids cleaning.
     *
     * @param parsed
     *         budget with source data
     * @param clean
     *         budget with clean data
     *
     * @return budget with cleaned data
     */
    @Override
    public final CleanBudgetItem clean(final ParsedBudgetItem parsed, final CleanBudgetItem clean) {
        if (clean.getBody() != null) {
            logger.debug("Cleaning body indetifiers for parsed budget {} starts", parsed.getId());

            clean.getBody().setBodyIds(ArrayUtils.walk(clean.getBody().getBodyIds(),
                n -> {
                    String id = n.getId();
                    // get last eight characters
                    if (id != null && id.length() > 8) {
                        n.setId(StringUtils.justifyLeft(id, 8, "0"));
                    }
                    
                    return n;
                }));
            
            logger.debug("Cleaning body identifiers for parsed budget {} finished", parsed.getId());
        }

        return clean;
    }
}
