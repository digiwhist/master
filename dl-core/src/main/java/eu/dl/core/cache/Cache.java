package eu.dl.core.cache;

/**
 * Defines basic cache functionality.
 */
public interface Cache {
    /**
     * Stores the item into cache.
     *
     * @param key key
     * @param value value
     */
    void put(String key, String value);

    /**
     * Retrieves the cached value.
     *
     * @param key key
     * @return cached value
     */
    String get(String key);
}
