package eu.dl.worker.master.plugin.generic.comparators;

import eu.dl.core.UnrecoverableException;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Method;
import java.time.LocalDateTime;
import java.util.Comparator;

/**
 * Latest date time comparator.
 *
 * @param <T> Child of Comparable
 *
 * @author Marek Mikes
 */
public class DateTimeComparator<T> implements Comparator<T> {

    private String fieldName;

    /**
     * Select what parameter is compared.
     *
     * @param fieldName parameter to compare
     */
    public DateTimeComparator(final String fieldName) {
        this.fieldName = StringUtils.capitalize(fieldName);
    }

    @Override
    public final int compare(final T o1, final T o2) {
        try {
            final Method getter = o1.getClass().getMethod("get" + fieldName);
            final LocalDateTime value1 = (LocalDateTime) getter.invoke(o1);
            final LocalDateTime value2 = (LocalDateTime) getter.invoke(o2);

            if (value1 == null && value2 == null) {
                return 0;
            }

            if (value1 == null && value2 != null) {
                return -1;
            }

            if (value1 != null && value2 == null) {
                return 1;
            }

            return value1.compareTo(value2);

        } catch (Exception e) {
            LoggerFactory.getLogger(this.getClass().getName()).error(
                    "Unable to compare values for field '{}' with exception {}", fieldName, e);
            throw new UnrecoverableException("Unable to compare values for exception", e);
        }
    }
}
