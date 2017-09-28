package eu.dl.worker.master.plugin;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.generic.Price;
import eu.dl.dataaccess.dto.matched.MasterablePart;
import eu.dl.worker.utils.BasePlugin;
import org.apache.commons.lang3.StringUtils;

import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Plugin class which takes the median from list of net amount prices and stores it into the final item if number
 * of prices is more than two. Otherwise LNN (last not null where "last" is according to publication date) is used.
 *
 * @param <T>
 *         item type to be mastered
 * @param <V>
 *         master item type
 * @param <U>
 *         root items - context
 */
public final class GeneralPricePlugin<T extends MasterablePart, V, U> extends BasePlugin
        implements MasterPlugin<T, V, U> {

    private List<String> fieldNames;

    /**
     * Initializes the plugin with field names to be mastered. The plugin will
     * call getFieldName and setFieldName methods on items.
     *
     * @param fieldNames
     *         field name to be mastered
     */
    public GeneralPricePlugin(final List<String> fieldNames) {
        super();
        this.fieldNames = fieldNames.stream().map(StringUtils::capitalize).collect(Collectors.toList());
    }

    /**
     * Struct which contains price and item from the input list.
     */
    private class PriceAndItem {
        private final BigDecimal price;
        private final T item;

        /**
         * @param price
         *         price
         * @param item
         *         item
         */
        PriceAndItem(final BigDecimal price, final T item) {
            this.price = price;
            this.item = item;
        }

        /**
         * @return price
         */
        BigDecimal getPrice() {
            return price;
        }

        /**
         * @return item where the price is
         */
        T getItem() {
            return item;
        }
    }

    /**
     * This comparator helps to order items in the price value order.
     */
    private class PriceComparator implements Comparator<PriceAndItem> {
        @Override
        public int compare(final PriceAndItem o1, final PriceAndItem o2) {
            assert o1.getPrice() != null && o2.getPrice() != null;
            return o1.getPrice().compareTo(o2.getPrice());
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

                List<PriceAndItem> sortedPrices = sortNetAmountPrices(items, getter);
                if (sortedPrices.size() <= 2) {
                    if (sortedPrices.size() == 1) {
                        T latestItem = sortedPrices.get(0).getItem();
                        Object latestPrice = getter.invoke(latestItem);
                        setter.invoke(finalItem, latestPrice);
                    } else if (sortedPrices.size() == 2) {
                        T latestItem = getLatestItem(sortedPrices.get(0), sortedPrices.get(1));
                        Object latestPrice = getter.invoke(latestItem);
                        setter.invoke(finalItem, latestPrice);
                    }
                } else {
                    int medianPriceIndex = getMedianPriceIndex(sortedPrices);
                    T medianItem = sortedPrices.get(medianPriceIndex).getItem();
                    Object medianPrice = getter.invoke(medianItem);
                    setter.invoke(finalItem, medianPrice);
                }
            } catch (Exception e) {
                // unable to pick the last value
                logger.error("Unable to pick the last value for field '{}' with exception {}", fieldName, e);
                throw new UnrecoverableException("Unable to pick value for exception", e);
            }
        }
        return finalItem;
    }

    /**
     * @param items
     *         items
     * @param getter
     *         getter
     *
     * @return list of sorted not null prices with its objects
     * @throws Exception
     *         in case that an error occurs during calling getter
     */
    private List<PriceAndItem> sortNetAmountPrices(final List<T> items, final Method getter) throws Exception {
        List<PriceAndItem> result = new ArrayList<>();

        // get not null prices
        for (T item : items) {
            Object priceObject = getter.invoke(item);
            if (priceObject != null) {
                assert priceObject instanceof Price;
                BigDecimal price = ((Price) priceObject).getNetAmount();
                if (price != null) {
                    result.add(new PriceAndItem(price, item));
                }
            }
        }

        return result.stream().sorted(new PriceComparator()).collect(Collectors.toList());
    }

    /**
     * @param sortedPrices
     *         sorted prices
     *
     * @return index of median
     */
    private int getMedianPriceIndex(final List<PriceAndItem> sortedPrices) {
        assert sortedPrices.size() > 2;
        return sortedPrices.size() % 2 == 0 ? sortedPrices.size() / 2 - 1 : sortedPrices.size() / 2;
    }

    /**
     * Method gets the latest item (tender, lot, bid) according to publication date.
     *
     * @param price1
     *         first price and item object which will be compared
     * @param price2
     *         second price and item object which will be compared
     *
     * @return latest item
     */
    private T getLatestItem(final PriceAndItem price1, final PriceAndItem price2) {
        T item1 = price1.getItem();
        T item2 = price2.getItem();
        LocalDate publicationDate1 = item1.getPublicationDate();
        LocalDate publicationDate2 = item2.getPublicationDate();
        if (publicationDate1 == null && publicationDate2 == null) {
            return item2;
        }
        if (publicationDate1 != null && publicationDate2 == null) {
            return item1;
        }
        if (publicationDate1 == null && publicationDate2 != null) {
            return item2;
        }
        return publicationDate2.isBefore(publicationDate1) ? item1 : item2;
    }
}
