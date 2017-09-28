package eu.dl.worker.master.plugin.generic.comparators;

import org.apache.commons.lang3.StringUtils;

import java.lang.reflect.Method;
import java.util.Comparator;

/**
 * Criteria comparator.
 *
 * @param <T> Child of Comparable
 *
 * @author Michal Riha
 */
public class StringComparator<T> implements Comparator<T> {

    private String fieldName;

    /**
     * Select what parameter is compared.
     *
     * @param fieldName parameter to compare
     */
    public StringComparator(final String fieldName) {
        this.fieldName = StringUtils.capitalize(fieldName);
    }

    @Override
    public final int compare(final T o1, final T o2) {
        try {
            final Method getter = o1.getClass().getMethod("get" + fieldName);

            final String value1 = (String) getter.invoke(o1);
            final String value2 = (String) getter.invoke(o2);

            if (value1 == null && value2 == null) {
                return 0;
            }

            if (value1 == null && value2 != null) {
                return -1;
            }

            if (value1 != null && value2 == null) {
                return 1;
            }

            return new Integer(value1.length()).compareTo(value2.length());

        } catch (Exception e) {
            e.printStackTrace();
        }

        return 0;
    }
}
