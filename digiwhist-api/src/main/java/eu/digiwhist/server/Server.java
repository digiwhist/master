package eu.digiwhist.server;

import static spark.Spark.before;
import static spark.Spark.exception;
import static spark.Spark.get;
import static spark.Spark.halt;
import static spark.Spark.options;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.Arrays;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import eu.digiwhist.dataaccess.dao.DAOFactory;
import eu.digiwhist.dataaccess.utils.PopulateUtils;
import eu.dl.core.config.Config;
import eu.dl.dataaccess.dao.CleanTenderDAO;
import eu.dl.dataaccess.dao.IndicatorDAO;
import eu.dl.dataaccess.dao.MasterBodyDAO;
import eu.dl.dataaccess.dao.MasterTenderDAO;
import eu.dl.dataaccess.dao.ParsedTenderDAO;
import eu.dl.dataaccess.dao.RawDataDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.clean.Cleanable;
import eu.dl.dataaccess.dto.master.MasterBid;
import eu.dl.dataaccess.dto.master.MasterBody;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import eu.dl.dataaccess.dto.master.Masterable;
import eu.dl.dataaccess.dto.parsed.Parsable;
import eu.dl.dataaccess.dto.raw.Raw;

/**
 * Main class for API.
 *
 * @author Kuba Krafka
 */
public final class Server {

    private static final String VERSION = "1";

    private static final String NAME = "server-api";

    private static final String DATE_FORMAT = "yyyy-MM-dd'T'HH:mm:ss.SSS";

    private static TransactionUtils transactionUtils;

    private static RawDataDAO rawDao;

    private static ParsedTenderDAO parsedDao;

    private static CleanTenderDAO cleanDao;

    private static MasterTenderDAO masterDao;
    
    private static MasterBodyDAO masterBodyDao;
    
    private static IndicatorDAO indicatorDao;

    private static final Logger logger = LoggerFactory.getLogger(Server.class);

    private static PopulateUtils populateUtils;

    /**
     * This class shouldn't be instantiated.
     */
    private Server() {
        // don't instantiate this class
    }

    /**
     * Starts the server.
     *
     * @param args
     *            CLI argument to be handed over
     */
    public static void main(final String[] args) {
        // new MainConfigFactory().build();
        final String configName = args[0];

        Config.getInstance().setConfigFile(Arrays.asList(configName));

        // init classes
        init();
        
        // configure api
        initApiOptions();

        registerRawTenderEndpoint();

        registerParsedTenderEndpoints();

        registerCleanTenderEndpoints();

        registerMasterTenderEndpoints();

        registerMaterBodyEndpoints();

        registerExceptionHandling();
    }

    /**
     * 
     */
    private static void registerRawTenderEndpoint() {
        // before("/*", new RequiresAuthenticationFilter(config,
        // "DirectBasicAuthClient"));
        get("/raw_tender/:id", "application/json", (request, response) -> {
            transactionUtils.begin();
            Raw result = rawDao.getById(request.params(":id"));
            transactionUtils.commit();
            return result;
        }, new JsonTransformer());
    }

    /**
     * 
     */
    private static void registerMaterBodyEndpoints() {

        get("/master_body/:id", "application/json", (request, response) -> {
            transactionUtils.begin();
            Masterable result = masterBodyDao.getById(request.params(":id"));
            transactionUtils.commit();
            return result;
        }, new JsonTransformer());

        get("/master_body/timestamp/:timestamp/page/:page", "application/json", (request, response) -> {
            LocalDateTime timestamp = getDate(request.params(":timestamp"));
            Integer page = getInteger(request.params(":page"));
            transactionUtils.begin();
            List<MasterBody> result = masterBodyDao.getModifiedAfter(timestamp, page);
            transactionUtils.commit();
            return result;
        }, new JsonTransformer());

        get("/master_body/timestamp/:timestamp", "application/json", (request, response) -> {
            LocalDateTime timestamp = getDate(request.params(":timestamp"));
            transactionUtils.begin();
            List<MasterBody> result = masterBodyDao.getModifiedAfter(timestamp, 1);
            transactionUtils.commit();
            return result;
        }, new JsonTransformer());
    }

    /**
     * 
     */
    private static void registerMasterTenderEndpoints() {
        get("/master_tender/timestamp/:timestamp/page/:page", "application/json", (request, response) -> {
            LocalDateTime timestamp = getDate(request.params(":timestamp"));
            Integer page = getInteger(request.params(":page"));
            transactionUtils.begin();
            List<MasterTender> result = masterDao.getModifiedAfter(timestamp, page);
            transactionUtils.commit();
            return populateUtils.populateIndicators(populateUtils.populateBodies(result));
        }, new JsonTransformer());

        get("/master_tender/country/:country/page/:page", "application/json", (request, response) -> {
            transactionUtils.begin();
            Integer page = getInteger(request.params(":page"));
            List<MasterTender> result = masterDao.getByCountry(request.params(":country"), page);
            hideSecretValues(result);
            transactionUtils.commit();
            return populateUtils.populateIndicators(populateUtils.populateBodies(result));
        }, new JsonTransformer());

        get("/master_tender/timestamp/:timestamp/source/:source/page/:page", "application/json",
                (request, response) -> {
                    LocalDateTime timestamp = getDate(request.params(":timestamp"));
                    Integer page = getInteger(request.params(":page"));
                    String source = request.params(":source");
                    transactionUtils.begin();
                    List<MasterTender> result = masterDao.getModifiedAfter(timestamp, source, page);
                    hideSecretValues(result);
                    transactionUtils.commit();
                    return populateUtils.populateIndicators(populateUtils.populateBodies(result));
                }, new JsonTransformer());
    }

    /**
     * This methods hides specific values from public API.
     * 
     * @param result result set to be processed
     */
    private static void hideSecretValues(final List<MasterTender> result) {
        for (MasterTender tender : result) {
            if (tender.getLots() != null) {
                for (MasterTenderLot lot : tender.getLots()) {
                    lot.setRobustEstimatedPrice(null);
                    if (lot.getBids() != null) {
                        for (MasterBid bid : lot.getBids()) {
                            bid.setRobustPrice(null);
                        }
                    }
                }
            }
        }
    }

    /**
     * 
     */
    private static void registerCleanTenderEndpoints() {
        get("/clean_tender/:id", "application/json", (request, response) -> {
            transactionUtils.begin();
            Cleanable result = cleanDao.getById(request.params(":id"));
            transactionUtils.commit();
            return result;
        }, new JsonTransformer());

        get("/clean_tender/country/:country/page/:page", "application/json", (request, response) -> {
            transactionUtils.begin();
            Integer page = getInteger(request.params(":page"));
            List<CleanTender> result = cleanDao.getByCountry(request.params(":country"), page);
            transactionUtils.commit();
            return result;
        }, new JsonTransformer());

        get("/clean_tender/timestamp/:timestamp/page/:page", "application/json", (request, response) -> {
            LocalDateTime timestamp = getDate(request.params(":timestamp"));
            Integer page = getInteger(request.params(":page"));
            transactionUtils.begin();
            List<CleanTender> result = cleanDao.getModifiedAfter(timestamp, page);
            transactionUtils.commit();
            return result;
        }, new JsonTransformer());

        get("/clean_tender/timestamp/:timestamp/source/:source/page/:page", "application/json", (request, response) -> {
            LocalDateTime timestamp = getDate(request.params(":timestamp"));
            Integer page = getInteger(request.params(":page"));
            String source = request.params(":source");
            transactionUtils.begin();
            List<CleanTender> result = cleanDao.getModifiedAfter(timestamp, source, page);
            transactionUtils.commit();
            return result;
        }, new JsonTransformer());

        get("/clean_tender/timestamp/:timestamp", "application/json", (request, response) -> {
            LocalDateTime timestamp = getDate(request.params(":timestamp"));
            transactionUtils.begin();
            List<CleanTender> result = cleanDao.getModifiedAfter(timestamp, 1);
            transactionUtils.commit();
            return result;
        }, new JsonTransformer());
    }

    /**
     * 
     */
    private static void registerParsedTenderEndpoints() {
        get("/parsed_tender/:id", "application/json", (request, response) -> {
            transactionUtils.begin();
            Parsable result = parsedDao.getById(request.params(":id"));
            transactionUtils.commit();
            return result;
        }, new JsonTransformer());
    }

    /**
     * Formats the string to date according to formatter. Stops the execution
     * when the date cannot be parsed.
     * 
     * @param date
     *            string date representation
     * @return formatted date or throws 400 with description
     */
    private static LocalDateTime getDate(final String date) {

        try {
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern(DATE_FORMAT);
            return LocalDateTime.parse(date, formatter);
        } catch (DateTimeParseException e) {
            halt(400, "Unable to parse date(expected format \"yyyy-MM-dd'T'HH:mm:ss.SSS\") from timestamp " + date);
            throw e;
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
    private static Integer getInteger(final String integer) {
        try {
            return Integer.parseInt(integer);
        } catch (NumberFormatException e) {
            halt(400, "Unable to parse integer from " + integer);
            throw e;
        }
    }

    /**
     * Inits DAOs and similar.
     */
    private static void init() {
        transactionUtils = DAOFactory.getDAOFactory().getTransactionUtils();

        rawDao = DAOFactory.getDAOFactory().getRawTenderDAO(NAME, VERSION);

        parsedDao = DAOFactory.getDAOFactory().getParsedTenderDAO(NAME, VERSION);

        cleanDao = DAOFactory.getDAOFactory().getCleanTenderDAO(NAME, VERSION);

        masterDao = DAOFactory.getDAOFactory().getMasterTenderDAO(NAME, VERSION);

        masterBodyDao = DAOFactory.getDAOFactory().getMasterBodyDAO(NAME, VERSION);
        
        indicatorDao = DAOFactory.getDAOFactory().getIndicatorDAO(NAME, VERSION);

        populateUtils = new PopulateUtils(masterBodyDao, indicatorDao);
    }

    /**
     * API specific config.
     */
    private static void initApiOptions() {
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
    private static void registerExceptionHandling() {
        exception(Exception.class, (exception, request, response) -> {
            logger.debug("Execution of request {} ended with an exception {}", request.queryString(), exception);
            response.status(500);
            response.body("<h1>Exception occurred</h1><div>" + exception.getMessage() + "</div>");
        });
    }
}
