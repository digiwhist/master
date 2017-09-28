package eu.dl.worker.master.plugin.generic;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.matched.MasterablePart;
import eu.dl.dataaccess.dto.utils.DTOUtils;
import eu.dl.worker.master.plugin.MasterPlugin;
import eu.dl.worker.master.plugin.generic.converter.Converter;

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

/**
 * This plugin takes the last non empty value in the list of items and stores it
 * into the final item.
 *
 * @param <T>
 *         matched items type
 * @param <V>
 *         item type to be mastered
 * @param <U>
 *         context items type
 */
public class LastValuePlugin<T extends MasterablePart, V, U>
        extends GenericMasterPlugin implements MasterPlugin<T, V, U> {

    private Comparator<T> comparator;

    private Converter converter;


    /**
     * Initializes the plugin with field names to be mastered. The plugin will
     * call getFieldName and setFieldName methods on items in the order defined
     * by comparator.
     *
     * @param fieldNames
     *         field name to be mastered
     * @param comparator
     *         used to order the collection of items
     * @param converter
     *         used to convert values if needed
     */
    public LastValuePlugin(final List<String> fieldNames, final Comparator<T> comparator, final Converter converter) {
        super(fieldNames);
        this.comparator = comparator;
        this.converter = converter;
    }

    /**
     * Initializes the plugin with field names list to be mastered. The plugin
     * will call getFieldName and setFieldName methods on items in the order
     * defined by comparator.
     *
     * @param fieldName
     *         field name to be cleaned
     * @param comparator
     *         used to order the collection of items
     * @param converter
     *         used to convert values if needed
     */
    public LastValuePlugin(final String fieldName, final Comparator<T> comparator, final Converter converter) {
        this(Arrays.asList(fieldName), comparator, converter);
    }

    @Override
    public final V master(final List<T> items, final V finalItem, final List<U> context) {
        for (String fieldName : fieldNames) {
            // getter and setter methods
            Method getter;
            try {
                // getter method
                getter = items.get(0).getClass().getMethod("get" + fieldName);

                Object result;

                // pick the setter
                Method setter = null;
                for (Method methodFound : finalItem.getClass().getMethods()) {
                    if (methodFound.getName().equals("set" + fieldName)) {
                        setter = methodFound;
                        break;
                    }
                }
                
                // iterate over result set and "pick" the last nonempty value
                for (T item : items.stream().sorted(comparator.reversed()).collect(Collectors.toList())) {
                    result = getter.invoke(item);

                    // setter method
                    if (!DTOUtils.isEmpty(result)) {
                        // save tbe value, call the setter
                        setter.invoke(finalItem, converter.convert(result));
                        
                        // sorted in reversed order, we dont need to pick last value but first non null instead
                        break;
                    }
                }
            } catch (Exception e) {
                // unable to pick the last value
                logger.error("Unable to pick the last value for field '{}' with exception {}", fieldName, e);
                throw new UnrecoverableException("Unable to pick value for exception", e);
            }
        }
        return finalItem;
    }
}
