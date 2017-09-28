package eu.dl.worker.utils;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collector;
import java.util.stream.Collectors;


/**
 * Provides usefull methods for array processing.
 *
 * @author Tomas Mrazek
 */
public final class ArrayUtils {

    /**
     * Utility classes should not have default constructor.
     */
    private ArrayUtils() {

    }

    /**
     * For each not null element from the {@code parsedList} list of calls the given function. Each not null result of
     * the call is saved to the list of clean entities.
     *
     * @param <T>
     *      input entity class
     * @param <U>
     *      output entity class
     * @param list
     *      list of input entities
     * @param function
     *      lambda function that transforms the T entity to the U entity.
     * @return non-empty list of U entities, otherwise null
     */
    public static <T, U> List<U> walk(final List<T> list, final Function<T, U> function) {
        if (list == null || list.isEmpty()) {
            return null;
        }

        final List<U> result = list.stream()
            .filter(Objects::nonNull)                           // non-null T
            .map((parsedItem) -> function.apply(parsedItem))    // transform T to U
            .filter(Objects::nonNull)                           // non-null U
            .collect(Collectors.toList());

        return (!result.isEmpty() ? result : null);
    }

    /**
     * Distinct keys in stream.
     *
     * @param keyExtractor
     *      key extractor
     * @param <T>
     *      list item class
     * @return distinct result
     */
    public static <T> Predicate<T> distinct(final Function<? super T, ?> keyExtractor) {
        Map<Object, Boolean> seen = new ConcurrentHashMap<>();
        return t -> seen.putIfAbsent(keyExtractor.apply(t), Boolean.TRUE) == null;
    }

    /**
     * Distinct keys in stream using {@link Objects#toString(java.lang.Object)} as key extractor.
     *
     * @see ArrayUtils#distinct(java.util.function.Function)
     * @param <T>
     *      list item class
     * @return distinct result
     */
    public static <T> Predicate<T> distinct() {
        return distinct(Objects::toString);
    }

    /**
     * Collector used for collecting of all maxes in the list according to comparator.
     *
     * @param <T>
     *      list item class
     * @param comp
     *      comparator used for list item comparsion
     * @return collector that collects all items with maximal value
     */
    public static <T> Collector<T, ?, List<T>> max(final Comparator<? super T> comp) {
        return Collector.of(
            ArrayList::new,
            (list, t) -> {
                int c = list.isEmpty() ? 0 : comp.compare(t, list.get(0));
                if (list.isEmpty() || c == 0) {
                    list.add(t);
                } else if (c > 0) {
                    list.clear();
                    list.add(t);
                }
            },
            (list1, list2) -> {
                if (list1.isEmpty()) {
                    return list2;
                }
                if (list2.isEmpty()) {
                    return list1;
                }
                int r = comp.compare(list1.get(0), list2.get(0));
                if (r < 0) {
                    return list2;
                } else if (r > 0) {
                    return list1;
                } else {
                    list1.addAll(list2);
                    return list1;
                }
            });
    }
}
