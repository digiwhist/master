package eu.dl.worker.master.plugin.specific.corrigendum.executors;

import eu.dl.dataaccess.dto.codetables.CorrectionType;
import eu.dl.dataaccess.dto.generic.Corrigendum;
import eu.dl.dataaccess.dto.master.MasterTender;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Base correction executor. Defines/implements some necessary methods.
 *
 * @param <T>
 *     class of the corrected value
 */
public abstract class BaseCorrectionExecutor<T> implements CorrectionExecutor {
    protected final Logger logger = LoggerFactory.getLogger(this.getClass().getName());

    /**
     * Compares the given value to the included corrections values.
     *
     * @param t
     *      master tender
     * @param origin
     *      value to be compared.
     * @return TRUE if origin matches one of the previous values.
     */
    protected final boolean isEqualToIncludedCorrection(final MasterTender t, final T origin) {
        if (t.getCorrections() == null) {
            return false;
        }

        for (Corrigendum c : t.getCorrections()) {
            if (Boolean.TRUE.equals(c.getIsIncluded()) && c.getType() == getCorrectionType()
                    && (isEqual(getCorrectionOrigin(c), origin) || isEqual(getCorrectionReplacement(c), origin))) {
                return true;
            }
        }

        return false;
    }

    /**
     * @return correction type.
     */
    protected abstract CorrectionType getCorrectionType();

    /**
     * @param c
     *      correction
     * @return correction origin value
     */
    protected abstract T getCorrectionOrigin(Corrigendum c);

    /**
     * @param c
     *      correction
     * @return correction replacement value
     */
    protected abstract T getCorrectionReplacement(Corrigendum c);

    /**
     * @param o1
     *      first value for comparison
     * @param o2
     *      second value for comparison
     * @return TRUE if the given values are considered the same, otherwise FALSE
     */
    protected abstract boolean isEqual(T o1, T o2);
}
