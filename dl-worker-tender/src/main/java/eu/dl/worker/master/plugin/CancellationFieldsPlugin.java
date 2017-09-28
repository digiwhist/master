package eu.dl.worker.master.plugin;

import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.master.BaseMasterTenderLot;
import eu.dl.dataaccess.dto.matched.MasterablePart;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import eu.dl.dataaccess.dto.utils.DTOUtils;
import eu.dl.worker.master.plugin.generic.LastValuePlugin;
import eu.dl.worker.master.plugin.generic.converter.TenderConverter;
import eu.dl.worker.utils.BasePlugin;
import org.apache.commons.lang3.StringUtils;

import java.time.LocalDate;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Plugin class which masters cancellation fields of tender/lot:
 *  - tender has cancellationDate, cancellationReason and isWholeTenderCancelled.
 *  - lot has cancellationDate and cancellationReason.
 * Rules are following:
 *  1) last not null value (ordered by publication date)
 *  2) if there is newer CONTRACT AWARD then publication that we took into a consideration in step 1 then reset all
 *     values to null
 * Algorithm:
 *  - order items by publication date
 *  - get items from sorted list which are after last contract award
 *  - run LastValuePlugin on new item list
 *
 * @param <T>
 *         item type to be mastered
 * @param <V>
 *         master item type
 * @param <U>
 *         context items
 */
public final class CancellationFieldsPlugin<T extends MasterablePart, V extends BaseMasterTenderLot, U>
        extends BasePlugin implements MasterPlugin<T, V, U> {

    private List<String> fieldNames;

    /**
     * No empty constructor allowed.
     */
    protected CancellationFieldsPlugin() {
        // no empty constructor allowed.
    }

    /**
     * Initializes the plugin with field names to be mastered. The plugin will
     * call getFieldName and setFieldName methods on items in the order defined
     * by comparator.
     *
     * @param fieldNames
     *         field name to be mastered
     */
    public CancellationFieldsPlugin(final List<String> fieldNames) {
        super();
        this.fieldNames = fieldNames.stream().map(StringUtils::capitalize).collect(Collectors.toList());
    }

    /**
     * This comparator helps to order lots in the publication date order.
     */
    final class PublicationDateComparator implements Comparator<T> {

        @Override
        public int compare(final T o1, final T o2) {
            LocalDate o1Date = o1.getPublicationDate();
            LocalDate o2Date = o2.getPublicationDate();

            if (o1Date == null && o2Date == null) {
                return 0;
            }

            if (o1Date == null) {
                return -1;
            }

            if (o2Date == null) {
                return 1;
            }

            return o1Date.compareTo(o2Date);
        }
    }

    @Override
    public V master(final List<T> items, final V finalItem, final List<U> context) {
        // sort by publication date
        final List<T> sortedItems = items
                .stream()
                .sorted(new PublicationDateComparator())
                .collect(Collectors.toList());


        // get sorted items after last contract award
        List<T> sortedItemsAfterLastContractAward = null;
        for (int i = sortedItems.size() - 1; i > 0; --i) {
            PublicationFormType publicationFormType = DTOUtils.getPublicationFormType(sortedItems.get(i),
                    (List<MatchedTender>) context);
            if (publicationFormType == PublicationFormType.CONTRACT_AWARD) {
                sortedItemsAfterLastContractAward = sortedItems.subList(i + 1, sortedItems.size());
                break;
            }
        }
        if (sortedItemsAfterLastContractAward == null) {
            // no contract award is in sorted list -> get the whole sorted list
            sortedItemsAfterLastContractAward = sortedItems;
        }

        // set last not null cancellation values to final item
        if (sortedItemsAfterLastContractAward.isEmpty()) {
            return finalItem;
        } else {
            LastValuePlugin<T, V, U> plugin = new LastValuePlugin<>(fieldNames,
                    new PublicationDateComparator(), new TenderConverter());
            return plugin.master(sortedItemsAfterLastContractAward, finalItem, context);
        }
    }
}
