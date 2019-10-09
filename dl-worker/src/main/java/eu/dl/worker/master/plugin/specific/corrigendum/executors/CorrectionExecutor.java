package eu.dl.worker.master.plugin.specific.corrigendum.executors;

import eu.dl.dataaccess.dto.generic.Corrigendum;
import eu.dl.dataaccess.dto.master.MasterTender;

/**
 * Correction executor interface. Defines fundamental method execute which is used to applying correction.
 */
public interface CorrectionExecutor {
    /**
     * Method applies correction {@code c} on the given tender {@code t}.
     *
     * @param c
     *      correction to be applied
     * @param t
     *      tender to be corrected
     */
    void execute(Corrigendum c, MasterTender t);
}
