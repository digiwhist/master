package eu.dl.worker.utils;

import org.silvertunnel_ng.netlib.adapter.url.URLGlobalUtil;
import org.silvertunnel_ng.netlib.api.NetFactory;
import org.silvertunnel_ng.netlib.api.NetLayer;
import org.silvertunnel_ng.netlib.api.NetLayerIDs;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class provides functionality related to underlyiong network. For example
 * redirects all HTTP traffic via TOR etc.
 */
public class NetworkUtils {
    private static final Logger logger = LoggerFactory.getLogger(NetworkUtils.class.getName());

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
        logger.debug("Starting TOR service");

        NetLayer lowerNetLayer = NetFactory.getInstance().getNetLayerById(NetLayerIDs.TOR);

        // wait until TOR is ready (optional):
        lowerNetLayer.waitUntilReady();

        // redirect URL handling (JVM global)
        URLGlobalUtil.initURLStreamHandlerFactory();

        // the following method could be called multiple times
        // to change layer used by the global factory over the time:
        URLGlobalUtil.setNetLayerUsedByURLStreamHandlerFactory(lowerNetLayer);
        logger.debug("TOR service started, HTTP traffic will be redirected via TOR from now on.");
    }

    /**
     * This method disables Tor usage for http traffic.
     */
    public static void disableTorForHttp() {
        logger.debug("Stopping TOR service");

        NetLayer lowerNetLayer = NetFactory.getInstance().getNetLayerById(NetLayerIDs.TCPIP);

        // wait until TOR is ready (optional):
        lowerNetLayer.waitUntilReady();

        // redirect URL handling (JVM global)
        URLGlobalUtil.initURLStreamHandlerFactory();

        // the following method could be called multiple times
        // to change layer used by the global factory over the time:
        URLGlobalUtil.setNetLayerUsedByURLStreamHandlerFactory(lowerNetLayer);

        logger.debug("TOR service stopped.");
    }
}
