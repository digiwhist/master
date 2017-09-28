package eu.dl.worker.master.plugin.specific;

import eu.dl.dataaccess.dto.generic.AwardCriterion;
import eu.dl.dataaccess.dto.master.BaseMasterTenderLot;
import eu.dl.dataaccess.dto.matched.BaseMatchedTenderLot;
import eu.dl.worker.master.plugin.generic.converter.TenderConverter;
import eu.dl.worker.master.plugin.specific.comparator.AwardCriteriaComparator;
import eu.dl.worker.master.plugin.MasterPlugin;
import eu.dl.worker.master.plugin.generic.LastPublishedPlugin;
import eu.dl.worker.utils.BasePlugin;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * This plugin takes the last published complete award criteria, or the most complete award criteria.
 *
 * @param <T>
 *         matched items type
 * @param <V>
 *         item type to be mastered
 * @param <U>
 *         context items type
 */
public class AwardCriteriaPlugin<T extends BaseMatchedTenderLot, V extends BaseMasterTenderLot,
        U extends BaseMatchedTenderLot>
        extends BasePlugin implements MasterPlugin<T, V, U> {

    private String fieldName = "awardCriteria";

    @SuppressWarnings("unchecked")
    @Override
    public final V master(final List<T> matchedTenders, final V finalItem, final List<U> context) {
        // filter criteria with sum of weights equal 100 if there are any
        final List<BaseMatchedTenderLot> tendersWithCompleteCriteria = new ArrayList<>();
        for (T matchedTender : matchedTenders) {
            if (matchedTender != null && matchedTender.getAwardCriteria() != null && matchedTender.getAwardCriteria()
                    .stream().mapToInt(t -> ((AwardCriterion) t).getWeight() == null ? 0 : ((AwardCriterion) t)
                            .getWeight()).sum() == 100) {
                tendersWithCompleteCriteria.add(matchedTender);
            }
        }

        // if there are criteria with sum of wights equal 100 choose the last published, if not, choose highest
        if (!tendersWithCompleteCriteria.isEmpty()) {
            return (V) new LastPublishedPlugin<BaseMatchedTenderLot, BaseMasterTenderLot, U>(
                    fieldName, new TenderConverter())
                    .master(tendersWithCompleteCriteria, finalItem, context);
        } else {
            // list is sorted in ascending order, save the last one
            final List<T> sortedMatchedTenders = matchedTenders.stream().sorted(
                    new AwardCriteriaComparator<>()).collect(Collectors.toList());

            finalItem.setAwardCriteria(sortedMatchedTenders.get(sortedMatchedTenders.size() -1).getAwardCriteria());

            return finalItem;
        }
    }
}
