package eu.dl.worker.master.plugin.generic;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.matched.MasterablePart;
import eu.dl.worker.master.plugin.MasterPlugin;

import java.lang.reflect.Method;
import java.util.List;

/**
 * This plugin makes logical OR of boolean fields of all items in the list and
 * the result stores in the final item. Logical OR is evaluated by following
 * way: <br>
 * (1) if at least one value is {@code TRUE} then master value is
 * {@code TRUE}<br>
 * (2) if all values are {@code FALSE} then master value is {@code FALSE}<br>
 * (3) if all values are {@code NULL} then master value is {@code NULL}
 *
 * @param <T>
 *         item type to be mastered
 * @param <V>
 *         item type to be mastered
 * @param <U>
 *         context items type
 */
public class LogicalORPlugin<T extends MasterablePart, V, U>
        extends GenericMasterPlugin implements MasterPlugin<T, V, U> {

    /**
     * Plugin initialization.
     *
     * @param fields
     *         field name to be mastered
     */
    public LogicalORPlugin(final List<String> fields) {
        super(fields);
    }

    @Override
    public final V master(final List<T> items, final V finalItem, final List<U> context) {
        if (items.isEmpty()) {
            return finalItem;
        }

        for (String field : fieldNames) {
            try {
                Method getter = items.get(0).getClass().getMethod("get" + field);
                Method setter = finalItem.getClass().getMethod("set" + field, Boolean.class);

                if (!getter.getReturnType().equals(Boolean.class)) {
                    logger.error("Field {} isn't declared as Boolean.", field);
                    throw new UnrecoverableException("Field isn't declared as Boolean.");
                }
  
                Boolean finalValue = null;
                for (T item : items) {                    
                    final Boolean itemValue = (Boolean) getter.invoke(item);
                    if (itemValue != null) {
                        if (itemValue) {
                            finalValue = true;
                            break;
                        } else {
                            finalValue = false;
                        }
                    }
                }

                setter.invoke(finalItem, finalValue);
            } catch (Exception e) {
                logger.error("Unable to master boolean value for field '{}' because of exception", field, e);
                throw new UnrecoverableException("Unable to master boolean value because of exception", e);
            }
        }

        return finalItem;
    }
}
