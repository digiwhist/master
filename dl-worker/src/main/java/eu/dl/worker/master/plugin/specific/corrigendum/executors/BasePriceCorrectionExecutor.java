package eu.dl.worker.master.plugin.specific.corrigendum.executors;

import eu.dl.dataaccess.dto.generic.Corrigendum;
import eu.dl.dataaccess.dto.generic.Price;
import org.apache.commons.lang3.ObjectUtils;

/**
 * Base correction executor which applies corrections of price fields.
 */
public abstract class BasePriceCorrectionExecutor extends BaseCorrectionExecutor<Price> {
    @Override
    protected final Price getCorrectionOrigin(final Corrigendum c) {
        return c.getOriginalValue();
    }

    @Override
    protected final Price getCorrectionReplacement(final Corrigendum c) {
        return c.getReplacementValue();
    }

    /**
     * Checks if prices are equal.
     *
     * @param o1
     *      price
     * @param o2
     *      price
     * @return true if prices are equal.
     */
    @Override
    protected final boolean isEqual(final Price o1, final Price o2) {
        if (ObjectUtils.allNotNull(o1.getNetAmount(), o2.getNetAmount()) && o1.getNetAmount().compareTo(o2.getNetAmount()) == 0) {
            return true;
        }
        return false;
    }
}
