package eu.dl.core.cache;

import eu.dl.core.config.Config;
import io.lettuce.core.RedisClient;
import io.lettuce.core.RedisURI;
import io.lettuce.core.api.StatefulRedisConnection;
import io.lettuce.core.api.sync.RedisCommands;

/**
 *
 */
public final class RedisCache implements Cache {
    protected String prefix = "";

    protected RedisClient client;

    protected StatefulRedisConnection<String, String> connection;

    protected RedisCommands<String, String> commands;

    /**
     * Redis cache initialisation.
     *
     * @param cachePrefix prefix for keys
     */
    public RedisCache(final String cachePrefix) {
        prefix = cachePrefix;

        Config config = Config.getInstance();
        String port= config.getParam("cache.redis.port");
        String host = config.getParam("cache.redis.host");

        RedisURI.Builder uri = RedisURI.Builder.redis(host);
        if (port != null) {
            uri.withPort(Integer.valueOf(port));
        }

        client = RedisClient.create(uri.build());

        connection = client.connect();

        commands = connection.sync();
    }

    @Override
    public void put(final String key, final String value) {
        commands.set(prefix.concat(key), value);
    }

    @Override
    public String get(final String key) {
        return commands.get(prefix.concat(key));
    }
}
