package eu.dl.core.cache;

import org.cache2k.Cache2kBuilder;

/**
 *
 */
public final class Cache2kCache implements Cache {
    protected final org.cache2k.Cache<String, String> cache = new Cache2kBuilder<String, String>() {}
            .name("cache")
            .eternal(true)
            .entryCapacity(180000000)
            .build();

    protected String prefix = "";

    /**
     * Constructs cache.
     * @param cachePrefix prefix used for keys
     */
    public Cache2kCache(final String cachePrefix) {
        this.prefix = cachePrefix;
    }

    @Override
    public void put(final String key, final String value) {
        cache.put(prefix.concat(key), value);
    }

    @Override
    public String get(final String key) {
        return cache.peek(prefix.concat(key));
    }
}
