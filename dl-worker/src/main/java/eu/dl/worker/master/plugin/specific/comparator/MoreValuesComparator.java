package eu.dl.worker.master.plugin.specific.comparator;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Comparator;

/**
 * This plugin compares number of filled variables in two objects.
 */
public class MoreValuesComparator implements Comparator {

    @Override
    public final int compare(final Object o1, final Object o2) {
        final Integer o1FieldsCount = countValues(o1);
        final Integer o2FieldsCount = countValues(o2);

        return o1FieldsCount.compareTo(o2FieldsCount);
    }

    /**
     * Get count of non null fields in object.
     *
     * @param object object of which to count non null values
     * @return count
     */
    private int countValues(final Object object) {
        int valueCount = 0;

        for (Method method : object.getClass().getMethods()) {
            if (method.getName().startsWith("get")) {
                try {
                    if (method.invoke(object) != null) {
                        valueCount++;
                    }
                } catch (IllegalAccessException | InvocationTargetException e) {
                    e.printStackTrace();
                }
            }
        }

        return valueCount;
    }
}
