package eu.dl.worker.utils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.BiPredicate;
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
     * Distinct keys in stream. If keyExtractor returns null, the item is kept in stream.
     *
     * @param keyExtractor
     *      key extractor
     * @param <T>
     *      list item class
     * @return distinct result
     */
    public static <T> Predicate<T> distinct(final Function<? super T, ?> keyExtractor) {
        Map<Object, Boolean> seen = new ConcurrentHashMap<>();
        return t -> {
            Object key = keyExtractor.apply(t);
            return key == null || seen.putIfAbsent(key, Boolean.TRUE) == null;
        };
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

    /**
     * Returns all entries from the list {@code a} which are presented in list {@code b}.
     *
     * @param <T>
     *      class of list entries
     * @param a
     *      list a
     * @param b
     *      list b
     * @param comp
     *      comparator of the list entries, returns true for same entries
     * @return all entries from the list a presented in the list b, otherwise empty list
     */
    public static <T> List<T> intersection(final List<T> a, final List<T> b, final BiPredicate<T, T> comp) {
        if (a == null || b == null || a.isEmpty() || b.isEmpty()) {
            return Collections.emptyList();
        }
        
        List<T> intersection = new ArrayList<>();
        a.forEach(aa -> {
            if (b.stream().anyMatch(bb -> comp.test(aa, bb))) {
                intersection.add(aa);
            }
        });

        return intersection;
    }

    /**
     * Returns list {@code a} complemented by all entries from list {@code b} which aren't presented in list a.
     *
     * @param <T>
     *      class of list entries
     * @param a
     *      list a
     * @param b
     *      list b
     * @param comp
     *      comparator of the list entries, returns true for same entries
     * @return list a complemented by all entries from list b which aren't presented in list a, otherwise empty list
     */
    public static <T> List<T> union(final List<T> a, final List<T> b, final BiPredicate<T, T> comp) {
        if (a == null && b == null) {
            return Collections.emptyList();
        } else if (a == null) {
            return b;
        } else if (b == null) {
            return a;
        }

        List<T> union = new ArrayList<>(a);
        
        b.forEach(bb -> {
            if (a.stream().noneMatch(aa -> comp.test(aa, bb))) {
                union.add(bb);
            }
        });

        return union;
    }
}
