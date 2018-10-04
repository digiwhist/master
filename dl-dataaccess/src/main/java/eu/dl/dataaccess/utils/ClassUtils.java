package eu.dl.dataaccess.utils;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.clean.Validable;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import org.apache.commons.lang3.StringUtils;

/**
 * Utils class which provides useful methods working with objects, classes, reflections etc.
 *
 * @author Tomas Mrazek
 */
public final class ClassUtils {

    private static final Logger logger = LoggerFactory.getLogger(ClassUtils.class);

    /**
     * Suppress default constructor for noninstatiability.
     */
    private ClassUtils() {
    }

    /**
     * @param <T>
     *      class of the input
     * @param input
     *      input to be tested
     * @return original value only and only if the call of input.isNonsense() returns false, otherwise null
     */
    public static <T> T removeNonsenses(final T input) {
        if (input == null) {
            return null;
        } else if (input instanceof Validable) {
            return (T) ((Validable) input).getValid();
        } else if (input instanceof String) {
            return ((String) input).isEmpty() ? null : input;
        }

        return input;
    }

    /**
     * Removes all nonsenses from the given list.
     *
     * @param <T>
     *      class of the list items
     * @param list
     *      list to be cleaned
     * @return non-empty list or null
     */
    public static <T> List<T> removeNonsenses(final List<T> list) {
        if (list == null) {
            return null;
        }

        try {
            List newList = list.getClass().newInstance();
            for (T n : list) {
                n = removeNonsenses(n);
                if (n != null) {
                    newList.add(n);
                }
            }

            return newList.isEmpty() ? null : newList;
        } catch (IllegalAccessException | InstantiationException ex) {
            logger.error("Unable to get new instance of the {} because of", list.getClass(), ex);
            throw new UnrecoverableException("Unable to get new instance of the collection");
        }
    }

    /**
     * Returns object property value if exists, otherwise null.
     *
     * @param <T>
     *      class of the underlying object
     * @param <U>
     *      class of the property value
     * @param clazz
     *      underlying object in which the property is looked for
     * @param property
     *      dot separated path to the property  (e.g. paht.to.the.property, array.0.item, ...)
     * @return value of the property if it is presented or null
     */
    public static <T, U> U getProperty(final T clazz, final String property) {
        if (clazz == null || property == null) {
            return null;
        }
        
        String[] p = property.split("\\.");
        if (p.length == 0) {
            return null;
        }

        try {
            Object value;
            if (!(clazz instanceof List)) {
                Method m = clazz.getClass().getMethod("get" + StringUtils.capitalize(p[0]));
                value = m.invoke(clazz);
            } else {
                value = ((List) clazz).get(Integer.parseInt(p[0]));
            }

            return p.length > 1 ? getProperty(value, property.substring(property.indexOf(".") + 1)) : (U) value;
        } catch (NumberFormatException ex) {
            logger.error("Mallformed property path. The number index of the list item was expected but '{}' was given.",
                p[0]);
            return null;
        } catch (IndexOutOfBoundsException ex) {
            logger.error("The number index '{}' of the list item is out of bounds.", p[0]);
            return null;
        } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException
            | SecurityException ex) {
            logger.error("Unable to get property '{}' because of", p[0], ex);
            return null;
        }
    }
}
