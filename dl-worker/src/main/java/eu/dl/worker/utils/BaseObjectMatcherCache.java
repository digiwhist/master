package eu.dl.worker.utils;

import java.util.Map;

/**
 * Interface for two-objects matching cache.
 *
 * @author Tomas Mrazek
 *
 * @param <T>
 *      class of item (left side in matching process), used for identification
 * @param <U>
 *      class of item (right side in matching process), cached item
 * @param <V>
 *      class of cache item
 */
public interface BaseObjectMatcherCache<T, U, V> {
    /**
     * Returns cache for the given item.
     *
     * @param t
     *      item for which the cache is looked for
     * @return cached item or null
     */
    U get(T t);

    /**
     * Caches item {@code u} for {@code t}.
     *
     * @param t
     *      item for which the cache to be saved
     * @param u
     *      cached data
     * @param metaData
     *      metaData
     */
    void set(T t, U u, Map<String, Object> metaData);

    /**
     * Invalidates cache for {@code t}.
     *
     * @param t
     *      item for which the cache to be invalidated
     */
    void invalidate(T t);

    /**
     * Returns raw cache record.
     *
     * @param t
     *      item for which the full cache is looked for
     * @return raw cache record or null
     */
    V getRaw(T t);
}
