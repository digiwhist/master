package eu.dl.worker.master.plugin.specific.comparator;

import eu.dl.dataaccess.dto.generic.AwardCriterion;
import eu.dl.dataaccess.dto.matched.BaseMatchedTenderLot;

import java.util.Comparator;
import java.util.List;

/**
 * Award Criteria comparator.
 *
 * @param <T> Tender or Lot
 */
public class AwardCriteriaComparator<T extends BaseMatchedTenderLot> implements Comparator<T> {
    @Override
    public final int compare(final T o1, final T o2) {
        @SuppressWarnings("unchecked") List<AwardCriterion> awardCriteria1 = o1.getAwardCriteria();
        @SuppressWarnings("unchecked") List<AwardCriterion> awardCriteria2 = o2.getAwardCriteria();

        if (awardCriteria1 == null && awardCriteria2 == null) {
            return 0;
        }

        if (awardCriteria1 == null && awardCriteria2 != null) {
            return -1;
        }

        if (awardCriteria1 != null && awardCriteria2 == null) {
            return 1;
        }

        Integer criteriaSum1 = awardCriteria1.stream().filter(t -> t.getWeight() != null)
                .mapToInt(AwardCriterion::getWeight).sum();
        Integer criteriaSum2 = awardCriteria2.stream().filter(t -> t.getWeight() != null)
                .mapToInt(AwardCriterion::getWeight).sum();

        return criteriaSum1.compareTo(criteriaSum2);
    }
}
