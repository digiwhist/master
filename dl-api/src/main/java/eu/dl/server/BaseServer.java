package eu.dl.server;


import eu.dl.core.UnrecoverableException;
import eu.dl.core.config.Config;
import eu.dl.server.exceptions.NotAuthenticatedException;
import eu.dl.server.exceptions.NotAuthorizedException;
import eu.dl.server.exceptions.NotFoundException;
import eu.dl.server.exceptions.ParameterFormattingException;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import org.mindrot.jbcrypt.BCrypt;
import org.pac4j.sparkjava.SecurityFilter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.Map;

import static spark.Spark.before;
import static spark.Spark.exception;
import static spark.Spark.internalServerError;
import static spark.Spark.notFound;
import static spark.Spark.options;
import static spark.Spark.post;
import static spark.Spark.secure;
import static spark.Spark.port;

/**
 * Main class for API.
 *
 * @author Kuba Krafka
 */
public abstract class BaseServer implements Server {

    private static final String DATE_FORMAT = "yyyy-MM-dd'T'HH:mm:ss.SSS";

    /**
     * Main logger.
     */
    protected final Logger logger = LoggerFactory.getLogger(this.getClass().getName());

    /**
     * Application config instance.
     */
    protected final Config config;

    protected final org.pac4j.core.config.Config serverConfig;

    protected final int port;

    /**
     * Initialization common for all the servers. Reads configuration variables,
     * prepares logging, prepares erro handling.
     */
    protected BaseServer() {
        config = Config.getInstance();

        String keyStoreLocation = config.getParam("authentication.keystoreFile");
        String keyStorePassword = config.getParam("authentication.keystorePassword");

        String apiPort = config.getParam("api.port");
        if (apiPort != null) {
            try {
                port = Integer.valueOf(apiPort);
            } catch (NumberFormatException ex) {
                logger.error("API port {} isn't an integer", apiPort);
                throw new UnrecoverableException("API port has to be an integer", ex);
            }
        } else {
            logger.error("API port can't be null");
            throw new UnrecoverableException("API port can't be null");
        }

        boolean isSecured = Boolean.valueOf(config.getParam("api.secured"));
        if (isSecured) {
            secure(keyStoreLocation, keyStorePassword, null, null);
        }

        serverConfig = new ServerConfigFactory().build();

        initApiOptions();

        registerSecurityEndpoints();

        registerErrorHandling();

        logger.debug("BaseServer initialised");
    }


    /**
     * Register endpoint relevant for authentication.
     */
    private void registerSecurityEndpoints() {
        before("/protected/*", new SecurityFilter(serverConfig, "parameterClient"));

        post("/login", (request, response) -> {
            String username = request.queryParams("username");
            String password = request.queryParams("password");

            String hash = config.getParam("authentication.users." + username);

            if (username == null || password == null) {
                throw new NotAuthenticatedException("Incomplete credentials provided, unable to authenticate.");
            }

            if (hash == null) {
                throw new NotAuthenticatedException("Unable to authenticate.");
            }

            if (BCrypt.checkpw(password, hash)) {
                response.type("application/json");
                response.status(200);
                String key = config.getParam("authentication.apiSecurityKey");

                Map<String, Object> claims = new HashMap<String, Object>();
                claims.put("expiration", LocalDateTime.now().plusHours(4).toString());

                String authToken = Jwts.builder()
                        .setSubject("authenticationToken")
                        .setClaims(claims)
                        .signWith(SignatureAlgorithm.HS512, key)
                        .compact();

                logger.debug("Generated new authentication token.");

                return String.format("{\"auth_token\":\"%s\"}", authToken);
            } else {
                throw new NotAuthenticatedException("Unable to verify user.");
            }
        });
    }

    /**
     * Formats the string to date according to formatter. Stops the execution
     * when the date cannot be parsed.
     *
     * @param date
     *            string date representation
     * @return formatted date or throws 400 with description
     */
    protected static LocalDateTime getDate(final String date) {
        try {
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern(DATE_FORMAT);
            return LocalDateTime.parse(date, formatter);
        } catch (Exception e) {
            throw new ParameterFormattingException("Unable to parse date(expected format " +
                    "\"yyyy-MM-dd'T'HH:mm:ss.SSS\") from timestamp " + date);
        }
    }

    /**
     * Formats the string to integer. Stops the execution when the int cannot be
     * parsed.
     *
     * @param integer
     *            string date representation
     * @return formatted integer or throws 400 with description
     */
    protected static Integer getInteger(final String integer) {
        try {
            return Integer.parseInt(integer);
        } catch (Exception e) {
            throw new ParameterFormattingException("Unable to parse integer from " + integer);
        }
    }

    /**
     * API specific config.
     */
    private void initApiOptions() {
        port(port);

        // allow access control origin, required or cross domain request from
        // browser
        options("/*", (request, response) -> {

            final String accessControlRequestHeaders = request.headers("Access-Control-Request-Headers");
            if (accessControlRequestHeaders != null) {
                response.header("Access-Control-Allow-Headers", accessControlRequestHeaders);
            }

            final String accessControlRequestMethod = request.headers("Access-Control-Request-Method");
            if (accessControlRequestMethod != null) {
                response.header("Access-Control-Allow-Methods", accessControlRequestMethod);
            }

            return "OK";
        });

        before((request, response) -> {
            response.header("Access-Control-Allow-Origin", "*");
        });
    }

    /**
     * Registers exception handling.
     */
    private void registerErrorHandling() {
        exception(ParameterFormattingException.class, (exception, request, response) -> {
            logger.debug("Execution of request {} ended with an exception {}", request.queryString(), exception);
            response.type("application/json");
            response.status(400);
            response.body(String.format("{\"message\":\"%s\"}", exception.getMessage()));
        });

        exception(NotFoundException.class, (exception, request, response) -> {
            logger.debug("Execution of request {} ended with an exception {}", request.queryString(), exception);
            response.type("application/json");
            response.status(404);
            response.body("{\"message\":\"Sorry, but no such item found.\"}");
        });

        notFound((request, response) -> {
            response.type("application/json");
            response.status(404);
            return "{\"message\":\"Sorry, but we haven't found you are asking for.\"}";
        });

        exception(NotAuthorizedException.class, (exception, request, response) -> {
            logger.debug("Execution of request {} ended with an exception {}", request.queryString(), exception);
            response.type("application/json");
            response.status(401);
            response.body("{\"message\":\"Sorry, but you are not authorized for this type of actions.\"}");
        });

        exception(NotAuthenticatedException.class, (exception, request, response) -> {
            logger.debug("Execution of request {} ended with an exception {}", request.queryString(), exception);
            response.type("application/json");
            response.status(401);
            response.body(String.format("{\"message\":\"Sorry, but something went wrong with authentication. %s\"}",
                    exception.getMessage()));
        });

        internalServerError((request, response) -> {
            logger.debug("Execution of request {} ended with an internal error", request.queryString());
            response.type("application/json");
            return "{\"message\":\"Something went wrong. A team of skilled unicorns has been called to duty. " +
                    "Please, keep calm and relax.\"}";
        });

    }
}
