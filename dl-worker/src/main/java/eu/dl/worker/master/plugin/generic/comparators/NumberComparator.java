package eu.dl.worker.master.plugin.generic.comparators;

import eu.dl.core.UnrecoverableException;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.util.Comparator;

/**
 * Bigger Number comparator.
 *
 * @param <T> Child of Comparable
 *
 * @author Michal Riha
 */
public class NumberComparator<T> implements Comparator<T> {

    private String fieldName;

    /**
     * Select what parameter is compared.
     *
     * @param fieldName parameter to compare
     */
    public NumberComparator(final String fieldName) {
        this.fieldName = StringUtils.capitalize(fieldName);
    }

    @Override
    public final int compare(final T o1, final T o2) {
        try {
            final Method getter = o1.getClass().getMethod("get" + fieldName);
            final Object object1 = getter.invoke(o1);
            final Object object2 = getter.invoke(o2);

            final BigDecimal value1;
            final BigDecimal value2;

            if (object1.getClass().equals(Integer.class)) {
                value1 = BigDecimal.valueOf((Integer) object1);
                value2 = BigDecimal.valueOf((Integer) object2);
            } else if (object1.getClass().equals(BigDecimal.class)) {
                value1 = (BigDecimal) object1;
                value2 = (BigDecimal) object2;
            } else {
                return 0;
            }

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
            // unable to pick the last value
            LoggerFactory.getLogger(this.getClass().getName()).error("Unable to pick the last " +
                    "value for field '{}' with exception {}", fieldName, e);
            throw new UnrecoverableException("Unable to pick value for exception", e);
        }
    }
}
