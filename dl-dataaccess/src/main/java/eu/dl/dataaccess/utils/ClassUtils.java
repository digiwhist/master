package eu.dl.dataaccess.utils;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.clean.Validable;

/**
 * Utils class which provides useful methods working with objects, classes, reflections etc.
 *
 * @author Tomas Mrazek
 */
public final class ClassUtils {

    private static final Logger logger = LoggerFactory.getLogger(ClassUtils.class);

    /**
     * Supress default constructor for noninstatiability.
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
}
