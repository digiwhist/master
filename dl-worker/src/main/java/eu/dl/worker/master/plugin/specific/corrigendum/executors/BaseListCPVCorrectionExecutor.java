package eu.dl.worker.master.plugin.specific.corrigendum.executors;

import eu.dl.dataaccess.dto.generic.CPV;
import eu.dl.dataaccess.dto.generic.Corrigendum;

import java.util.List;

/**
 * Base correction executor which applies corrections of CPVs.
 */
public abstract class BaseListCPVCorrectionExecutor extends SimpleCorrectionExecutor<List<CPV>> {
    /**
     * @param c correction
     * @return correction origin value
     */
    @Override
    protected final List<CPV> getCorrectionOrigin(final Corrigendum c) {
        return c.getOriginalCpvs();
    }

    /**
     * @param c correction
     * @return correction replacement value
     */
    @Override
    protected final List<CPV> getCorrectionReplacement(final Corrigendum c) {
        return c.getReplacementCpvs();
    }

    /**
     * @param o1 master tender
     * @param o2 master tender
     * @return true if o1 is equal to o2 and false if not.
     */
    @Override
    protected final boolean isEqual(final List<CPV> o1, final List<CPV> o2) {
        if (o1 == null) {
            return o2 == null;
        }
        if (o2 == null) {
            return false;
        }

        if (o1.size() != o2.size()) {
            return false;
        }
        for (int i = 0; i < o1.size(); i++) {
            if (!(o1.get(i).getCode().equals(o2.get(i).getCode()))) {
                return false;
            }
        }
        return true;
    }


}
