package eu.dl.dataaccess.utils;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Utils class which provides useful methods for object validation.
 *
 * @author Tomas Mrazek
 */
public final class ValidationUtils {

    private static final Logger logger = LoggerFactory.getLogger(ValidationUtils.class);

    /**
     * Suppress default constructor for noninstatiability.
     */
    private ValidationUtils() {
    }

    /**
     * Validates given object {@code o} on the base of passed values.
     *
     * @param <T>
     *      class of the underlying object
     * @param <U>
     *      value class
     * @param o
     *      underlying object to be validated
     * @param values
     *      values to be tested. They should come from underlying object but in some special cases we can make decision
     *      about the object validity on the base of foreign values.
     * @return underlying object {@code o} in case that any of passed values is not null, otherwise returns null and
     *      logs warning message about removing.
     */
    public static <T, U> T getValid(final T o, final U... values) {
        if (Arrays.asList(values).stream().anyMatch(n -> n != null)) {
            return o;
        }

        logger.warn("VALIDATION - invalid {} has been removed", o.getClass().getCanonicalName());
        return null;
    }

    /**
     * In very first removes nonsenses from the given {@code list} and then validates it.
     *
     * @see ClassUtils#removeNonsenses(java.util.List)
     * @param <T>
     *      class of the list items
     * @param list
     *      list to be validated
     * @return non-empty list without nonsenses or returns null and logs warning message about removing.
     */
    public static <T> List<T> getValid(final List<T> list) {
        if (list != null) {
            int len = list.size();

            String itemClass = list.stream()
                .filter(Objects::nonNull)
                .findFirst()
                .map(n -> n.getClass().getCanonicalName())
                .orElse("unknown");

            List<T> newList = ClassUtils.removeNonsenses(list);

            if (newList == null) {
                logger.warn("VALIDATION - whole List<{}> has been removed", itemClass);
            } else if (newList.size() < len) {
                logger.warn("VALIDATION - " + (len - newList.size()) + " items have been removed from the List<{}>",
                    itemClass);
            }

            return newList;
        }

        return null;
    }
}
