package eu.dl.worker.master.plugin;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.generic.Address;
import eu.dl.dataaccess.dto.matched.MasterablePart;
import eu.dl.worker.utils.BasePlugin;
import org.apache.commons.lang3.StringUtils;

import java.lang.reflect.Method;
import java.time.LocalDate;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Plugin class which masters address.
 * The whole address object is selected, individual fields are not merged.
 * Scoring: NUTS has priority, otherwise number of non-null.
 * In case of the same score:
 *   1) we know publication date (tender/lot): take the last one according to publication date.
 *   2) we do not know publication date (body): take the last one according to input object list order.
 *
 * @param <T>
 *         item type to be mastered
 * @param <V>
 *         master item type
 * @param <U>
 *         root items - context
 */
public final class AddressPlugin<T extends MasterablePart, V, U> extends BasePlugin implements MasterPlugin<T, V, U> {

    /**
     * Plugin name.
     */
    public static final String PLUGIN_ID = "AddressPlugin";

    private List<String> fieldNames;

    /**
     * No empty constructor allowed.
     */
    private AddressPlugin() {
        // no empty constructor allowed.
    }

    /**
     * Initializes the plugin with field names to be mastered. The plugin will
     * call getFieldName and setFieldName methods on items.
     *
     * @param fieldNames
     *         field name to be mastered
     */
    public AddressPlugin(final List<String> fieldNames) {
        super();
        this.fieldNames = fieldNames.stream().map(StringUtils::capitalize).collect(Collectors.toList());
    }

    /**
     * This comparator helps to order addresses in its score value order. In case of the same score, take the last one
     * according to publication date.
     * Scoring: NUTS has priority, otherwise number of non-null
     */
    final class AddressComparator implements Comparator<T> {
        private Method getter;

        /**
         * @param getter
         *         method to get address
         */
        AddressComparator(final Method getter) {
            this.getter = getter;
        }

        @Override
        public int compare(final T o1, final T o2) {
            Address address1;
            Address address2;

            // get addresses
            try {
                address1 = (Address) getter.invoke(o1);
                address2 = (Address) getter.invoke(o2);
                if (address1 == null && address2 == null) {
                    return 0;
                }
                if (address1 == null) {
                    return -1;
                }
                if (address2 == null) {
                    return 1;
                }
            } catch (Exception e) {
                // unable to pick the last value
                logger.error("Unable to get address with exception {}", e);
                throw new UnrecoverableException("Unable to get address for exception", e);
            }

            // addresses are not null

            int address1Score = getScore(address1);
            int address2Score = getScore(address2);

            if (address1Score < address2Score) {
                return -1;
            } else if (address1Score > address2Score) {
                return 1;
            } else {
                LocalDate publicationDate1 = o1.getPublicationDate();
                LocalDate publicationDate2 = o2.getPublicationDate();
                if (publicationDate1 == null && publicationDate2 == null) {
                    return 0;
                }
                if (publicationDate1 == null && publicationDate2 != null) {
                    return -1;
                }
                if (publicationDate1 != null && publicationDate2 == null) {
                    return 1;
                }
                return publicationDate1.compareTo(publicationDate2);

            }
        }

        /**
         * Returns score of the address - NUTS has priority, otherwise number of non-null.
         *
         * @param address
         *          address to be scored
         *
         * @return score of the address
         */
        int getScore(final Address address) {
            int score = 0;

            score += (address.getNuts() == null) ? 0 : (10 * address.getNuts().size());

            score += (address.getStreet() == null) ? 0 : 1;
            score += (address.getCity() == null) ? 0 : 1;
            score += (address.getPostcode() == null) ? 0 : 1;
            score += (address.getCountry() == null) ? 0 : 1;
            score += (address.getRawAddress() == null) ? 0 : 1;
            score += (address.getState() == null) ? 0 : 1;
            score += (address.getUrl() == null) ? 0 : 1;

            return score;
        }
    }

    @Override
    public V master(final List<T> items, final V finalItem, final List<U> context) {
        for (String fieldName : fieldNames) {
            try {
                // getter method
                Method getter = items.get(0).getClass().getMethod("get" + fieldName);

                // setter method
                Method setter = null;
                for (Method method : finalItem.getClass().getMethods()) {
                    if (method.getName().equals("set" + fieldName)) {
                        setter = method;
                        break;
                    }
                }
                assert setter != null;

                List<T> sortedItems = items
                        .stream()
                        .sorted(new AddressComparator(getter))
                        .collect(Collectors.toList());

                if (sortedItems.isEmpty()) {
                    continue;
                }
                Object masterAddress = getter.invoke(sortedItems.get(sortedItems.size() - 1));
                setter.invoke(finalItem, masterAddress);
            } catch (Exception e) {
                // unable to pick the last value
                logger.error("Unable to set address for field '{}' with exception {}", fieldName, e);
                throw new UnrecoverableException("Unable to set address for exception", e);
            }
        }
        return finalItem;
    }
}
