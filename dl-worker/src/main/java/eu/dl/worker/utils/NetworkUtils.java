package eu.dl.worker.utils;

import eu.dl.core.config.Config;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class provides functionality related to underlyiong network. For example
 * redirects all HTTP traffic via TOR etc.
 */
public class NetworkUtils {
    private static final Logger logger = LoggerFactory.getLogger(NetworkUtils.class.getName());

    private static final Config config = Config.getInstance();

    /**
     * Utility class only. No constructor needed here.
     */
    protected NetworkUtils() {

    }

    /**
     * This methods starts Tor connection and reconfigures JVM to use it for all
     * HTTP communication.
     */
    public static void enableTorForHttp() {
        logger.info("Starting TOR service");

        System.getProperties().put("proxySet", "true");
        System.getProperties().put("socksProxyHost", config.getParam("tor.socksProxyHost"));
        System.getProperties().put("socksProxyPort", config.getParam("tor.socksProxyPort"));
        System.getProperties().put("socksNonProxyHosts", config.getParam("tor.socksNonProxyHosts"));

        logger.info("TOR service started, HTTP traffic will be redirected via TOR from now on.");
    }

    /**
     * This method disables Tor usage for http traffic.
     */
    public static void disableTorForHttp() {
        logger.info("Stopping TOR service");

        System.getProperties().put("proxySet", "false");

        logger.info("TOR service stopped.");
    }
}
