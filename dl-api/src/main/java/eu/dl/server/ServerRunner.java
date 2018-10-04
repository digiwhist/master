package eu.dl.server;

import eu.dl.core.config.Config;
import org.eclipse.jetty.util.thread.QueuedThreadPool;
import org.eclipse.jetty.util.thread.ThreadPool;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import spark.embeddedserver.EmbeddedServers;
import spark.embeddedserver.jetty.EmbeddedJettyServer;
import spark.embeddedserver.jetty.JettyHandler;
import spark.embeddedserver.jetty.JettyServerFactory;
import spark.http.matching.MatcherFilter;
import spark.route.Routes;
import spark.staticfiles.StaticFilesConfiguration;

/**
 * This is runner for our workers. Should be used from CLI.
 *
 * @author Kuba Krafka
 */
final class ServerRunner {

    /**
     * Logger.
     */
    private static final Logger logger = LoggerFactory.getLogger(ServerRunner.class);

    /**
     * This class shouldn't be instantiated.
     */
    private ServerRunner() {
        // dont instantiate this class
    }

    /**
     * Entry point.
     *
     * @param args argument specifying worker name
     */
    public static void main(final String[] args) {
        String serverName = "";

        String configName = "";

        for (String arg : args) {
            if (arg.startsWith("eu.")) {
                //worker
                serverName = arg;
            } else {
                configName = arg;
            }
        }

        if (configName.isEmpty()) {
            logger.error("No configuration name provided");
            System.exit(1);
        }

        Config.getInstance().addConfigFile(configName);

        EmbeddedServers.add(EmbeddedServers.Identifiers.JETTY,
                            (Routes routeMatcher,
                             StaticFilesConfiguration staticFilesConfiguration,
                             boolean hasMultipleHandler) -> {
            JettyHandler handler = setupHandler(routeMatcher, staticFilesConfiguration, hasMultipleHandler);
            handler.getSessionCookieConfig().setName("XSESSION");
            return new EmbeddedJettyServer(new JettyServerFactory() {
                @Override
                public org.eclipse.jetty.server.Server create(final int maxThreads,
                                                              final int minThreads,
                                                              final int threadTimeoutMillis) {
                    org.eclipse.jetty.server.Server server = new org.eclipse.jetty.server.Server(
                            new QueuedThreadPool(20, 4, 30000));
                    server.setAttribute("org.eclipse.jetty.server.Request.maxFormContentSize", -1);
                    return server;
                }

                @Override
                public org.eclipse.jetty.server.Server create(final ThreadPool tp) {
                    throw new UnsupportedOperationException("Not supported yet.");
                }
            }, handler);
        });

        // get the worker via reflection
        try {
            @SuppressWarnings("rawtypes") final Class serverClass = Class.forName(serverName);
            final Server server = (Server) serverClass.newInstance();
            server.start();
            logger.info("Starting server {}", serverName);
        } catch (final ClassNotFoundException e) {
            logger.error("No such server found '{}' exception {}", serverName, e.toString());
            e.printStackTrace();
            System.exit(1);
        } catch (final InstantiationException e) {
            logger.error("Unable to instantiate the server '{}' exception {}", serverName, e.toString());
            e.printStackTrace();
            System.exit(1);
        } catch (final IllegalAccessException e) {
            logger.error("Unable to access the server '{}' exception {}", serverName, e.toString());
            e.printStackTrace();
            System.exit(1);
        }
    }

    /**
     * Setup of spark server.
     * @param routeMatcher route matcher
     * @param staticFilesConfiguration static files config
     * @param hasMultipleHandler has multiple handler
     * @return jetty handler
     */
    private static JettyHandler setupHandler(final Routes routeMatcher,
                                final StaticFilesConfiguration staticFilesConfiguration,
                                final boolean hasMultipleHandler) {
        MatcherFilter matcherFilter = new MatcherFilter(routeMatcher,
                staticFilesConfiguration,
                false, hasMultipleHandler);
        matcherFilter.init(null);

        return new JettyHandler(matcherFilter);
    }

}
