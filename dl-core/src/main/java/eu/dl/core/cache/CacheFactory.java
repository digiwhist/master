package eu.dl.core.cache;


import eu.dl.core.config.Config;
import eu.dl.core.config.MisconfigurationException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Returns intialised cache object.
 */
public class CacheFactory {

    private static Cache cache;

    private static Logger logger = LoggerFactory.getLogger(CacheFactory.class.getName());

    /**
     * Default constructor.
     */
    protected CacheFactory() {
        // no public constructor available
    }

    /**
     * Creates and returns cache.
     *
     * @param cachePrefix used for keys
     *
     * @return ready to use cache
     */
    public static final Cache getCache(final String cachePrefix) {
        if (cache == null) {
            Config config = Config.getInstance();
            String cacheImplementation = config.getParam("cache.implementation");
            if (cacheImplementation != null && cacheImplementation.equals("cache2k")) {
                logger.info("Returning cache2k cache");
                cache = new Cache2kCache(cachePrefix);
            } else if (cacheImplementation != null && cacheImplementation.equals("redis")) {
                logger.info("Returning redis cache");
                cache = new RedisCache(cachePrefix);
            } else {
                throw new MisconfigurationException(String.format("Improper cache configuration. %s cache not found", cacheImplementation));
            }
        }
        return cache;
    }
}
