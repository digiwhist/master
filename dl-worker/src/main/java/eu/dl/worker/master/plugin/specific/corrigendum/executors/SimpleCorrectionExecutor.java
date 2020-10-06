package eu.dl.worker.master.plugin.specific.corrigendum.executors;

import eu.dl.dataaccess.dto.generic.Corrigendum;
import eu.dl.dataaccess.dto.master.MasterTender;

/**
 * Simple correction executor - corrects single tender origin value with appropriate correction. Implements fundamental method execute
 * which is used to applying correction.
 *
 * @param <T>
 *     class of the corrected value
 */
public abstract class SimpleCorrectionExecutor<T> extends BaseCorrectionExecutor<T> {
    @Override
    public final void execute(final Corrigendum c, final MasterTender t) {
        // get origin/replacement value of an requested data type
        T origin = getCorrectionOrigin(c);
        T replacement = getCorrectionReplacement(c);
        // no replacement found, skip
        if (replacement == null) {
            return;
        }

        T tenderOrigin = getTenderOrigin(t);
        if (origin == null || tenderOrigin == null || isEqual(tenderOrigin, origin) || isEqualToIncludedCorrection(t, origin)) {
            applyReplacement(t, replacement);
            c.setIsIncluded(true);
            // Setting type based on sectionNumber.
            c.setType(getCorrectionType());
        } else {
            logger.warn("The correction wasn't executed because of replaced value is not equal to correction's original value " +
                "and to any of previous values.");
            c.setIsIncluded(false);
        }
    }

    /**
     * @param t
     *      master tender
     * @return tender origin value
     */
    protected abstract T getTenderOrigin(MasterTender t);

    /**
     * @param t
     *      master tender
     * @param replacement
     *      replacement value
     */
    protected abstract void applyReplacement(MasterTender t, T replacement);
}
