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
    public static void enableProxyForHttp() {
        logger.info("Configuring proxy service");

        System.getProperties().put("proxySet", "true");
        System.getProperties().put("http.proxyHost", config.getParam("proxy.host"));
        System.getProperties().put("http.proxyPort", config.getParam("proxy.port"));
        System.getProperties().put("http.nonProxyHosts", config.getParam("proxy.nonProxyHosts"));
        System.getProperties().put("https.proxyHost", config.getParam("proxy.host"));
        System.getProperties().put("https.proxyPort", config.getParam("proxy.port"));
        System.getProperties().put("https.nonProxyHosts", config.getParam("proxy.nonProxyHosts"));

        logger.info("Proxy service configured, HTTP traffic will be redirected via proxy from now on.");
    }

    /**
     * This method disables Tor usage for http traffic.
     */
    public static void disableProxyForHttp() {
        logger.info("Stopping proxy service");

        System.getProperties().put("proxySet", "false");

        logger.info("Proxy service stopped.");
    }
}
