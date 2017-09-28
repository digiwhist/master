package eu.dl.worker.master.plugin.generic;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.matched.MasterablePart;
import eu.dl.worker.master.plugin.MasterPlugin;

import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * This plugin takes the object with latest median value.
 *
 * @param <T>
 *           matched items type
 * @param <V>
 *           item type to be mastered
 * @param <U>
 *         context items type
 */
public class MedianPlugin<T extends MasterablePart, V, U> extends GenericMasterPlugin implements MasterPlugin<T, V, U> {

    /**
     * Initializes the plugin with field names to be mastered. The plugin will
     * call getFieldName and setFieldName methods on items in the order defined
     * by comparator.
     *
     * @param fieldNames field name to be mastered
     */
    public MedianPlugin(final List<String> fieldNames) {
        super(fieldNames);
    }

    /**
     * Initializes the plugin with field names list to be mastered. The plugin
     * will call getFieldName and setFieldName methods on items in the order
     * defined by comparator.
     *
     * @param fieldName field name to be cleaned
     */
    public MedianPlugin(final String fieldName) {
        this(Arrays.asList(fieldName));
    }

    @Override
    public final V master(final List<T> items, final V finalItem, final List<U> context) {
        for (String fieldName : fieldNames) {
            // getter and setter methods
            try {
                // getter method
                Method getter = items.get(0).getClass().getMethod("get" + fieldName);

                // get array of all numbers we want to get median from into one list
                List<BigDecimal> list = new ArrayList<>();
                for (T item : items) {
                    final Object numberObject = getter.invoke(item);
                    if (numberObject != null) {
                        final BigDecimal number = numberObject instanceof BigDecimal ? (BigDecimal) numberObject
                                : BigDecimal.valueOf((Integer) numberObject);
                        list.add(number);
                    }
                }

                // count median index if there is any
                int medianIndex;
                if (list.isEmpty()) {
                    return finalItem;
                } else if (list.size() == 1) {
                    medianIndex = 0;
                } else {
                    medianIndex = list.size() % 2 == 0 ? list.size() / 2 - 1 : list.size() / 2;
                }

                // sort list of numbers and get
                Collections.sort(list);
                BigDecimal result = list.get(medianIndex);

                if (null != result) {
                    for (Method method : finalItem.getClass().getMethods()) {
                        if (method.getName().equals("set" + fieldName)) {

                            // check if we must convert matched to master
                            if (getter.getReturnType().equals(Integer.class)) {
                                method.invoke(finalItem, result.intValue());
                            } else {
                                method.invoke(finalItem, result);
                            }

                            break;
                        }
                    }
                }
            } catch (Exception e) {
                // unable to pick the last value
                logger.error("Unable to pick the value for field '{}' with exception {}", fieldName, e);
                throw new UnrecoverableException("Unable to pick value for exception", e);
            }
        }

        return finalItem;
    }
}
