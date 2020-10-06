package eu.dl.worker.master.plugin.specific.corrigendum.executors;

import eu.dl.dataaccess.dto.codetables.CorrectionType;
import eu.dl.dataaccess.dto.generic.Corrigendum;
import eu.dl.dataaccess.dto.master.MasterTender;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Base correction executor. Implements fundamental method execute which is used to applying correction, and defines some necessary methods.
 *
 * @param <T> class of the corrected value
 */
public abstract class BaseCorrectionExecutor<T> implements CorrectionExecutor {
    private final Logger logger = LoggerFactory.getLogger(this.getClass().getName());

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
        if (origin == null || tenderOrigin == null || isEqual(tenderOrigin, origin) || isEqualToThePreviousValue(t, origin)) {
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
     * @return correction type.
     */
    protected abstract CorrectionType getCorrectionType();

    /**
     * @param c correction
     * @return correction origin value
     */
    protected abstract T getCorrectionOrigin(Corrigendum c);

    /**
     * @param c correction
     * @return correction replacement value
     */
    protected abstract T getCorrectionReplacement(Corrigendum c);

    /**
     * @param t master tender
     * @return tender origin value
     */
    protected abstract T getTenderOrigin(MasterTender t);

    /**
     * @param o1 master tender
     * @param o2 master tender
     * @return tender origin value
     */
    protected abstract boolean isEqual(T o1, T o2);

    /**
     * Compares the given value to the previous values.
     *
     * @param t      master tender
     * @param origin value to be compared.
     * @return true if origin matches one of the previous values.
     */
    protected abstract boolean isEqualToThePreviousValue(MasterTender t, T origin);

    /**
     * @param t           master tender
     * @param replacement replacement value
     */
    protected abstract void applyReplacement(MasterTender t, T replacement);
}
