package eu.digiwhist.server;

import eu.digiwhist.dataaccess.dao.DAOFactory;
import static spark.Spark.get;
import static spark.Spark.halt;
import static spark.Spark.options;
import static spark.Spark.exception;
import static spark.Spark.before;

import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import eu.digiwhist.dataaccess.utils.PopulateUtils;
import eu.dl.core.UnrecoverableException;
import eu.dl.core.config.Config;
import eu.dl.dataaccess.dao.CleanTenderDAO;
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
import eu.dl.dataaccess.dto.ocds.OCDSReleasePackage;
import eu.dl.dataaccess.dto.parsed.Parsable;
import eu.dl.dataaccess.dto.raw.Raw;
import eu.dl.dataaccess.dto.utils.OCDSUtils;

import java.math.BigDecimal;
import java.net.MalformedURLException;
import java.net.URL;
import java.time.LocalDate;
import java.time.Month;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static eu.dl.dataaccess.utils.RemoveNonsenseUtils.removeNonsensicalAmount;
import static eu.dl.dataaccess.utils.RemoveNonsenseUtils.removeNonsensicalDateTime;

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

    private static final Logger logger = LoggerFactory.getLogger(Server.class);

    private static PopulateUtils populateUtils;

    private static final BigDecimal PRICE_MAX = BigDecimal.valueOf(1200000000);

    private static final LocalDate DATE_MIN = LocalDate.of(2000, Month.JANUARY, 1);
    private static final LocalDate DATE_MAX = LocalDate.of(2025, Month.JANUARY, 1);

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
            List<MasterBody> result = masterBodyDao.getModifiedAfter(timestamp, 0);
            transactionUtils.commit();
            return result;
        }, new JsonTransformer());
    }


    /**
     * 
     */
    private static void registerMasterTenderEndpoints() {
        get("/master_tender/*", "application/json", (request, response) -> {
            // path parsing
            Matcher m = Pattern.compile("master_tender/"
                + "(:?(?<format>ocds|ocds\\-arr|opentender)/)?"
                + "("
                    // by id (page and/or source can be set but are ignored)
                    + "(?<id>[^/]+)"
                    // by country and page, and optionally by source
                    + "|country/(?<country>[^/]+)"
                    // by timestamp, page, and optionally by source
                    + "|timestamp/(?<timestamp>[^/]+)"
                + ")(:?/source/(?<source>[^/]+))?(:?/page/(?<page>[^/]+))?$").matcher(request.url());

            // unknown path
            if (!m.find()) {
                halt(404, "Not found");
                return null;
            }

            // get page from request path if exists or use first page (0)
            Integer page = m.group("page") != null ? getInteger(m.group("page")) : 0;

            // output formatting
            String format = m.group("format");
            
            // data retreiving according to path
            transactionUtils.begin();
            List<MasterTender> result = null;
            boolean returnFirst = false;
            if (m.group("id") != null) {
                returnFirst = true;
                MasterTender record = masterDao.getById(m.group("id"));
                if (record != null) {
                    result = Arrays.asList(record);
                }
            } else if (m.group("country") != null) {
                String country = m.group("country");
                String source = m.group("source");

                result = masterDao.getByCountry(country, page, source, "opentender".equals(format));
            } else if (m.group("timestamp") != null) {
                LocalDateTime timestamp = getDate(m.group("timestamp"));
                String source = m.group("source");

                result = masterDao.getModifiedAfter(timestamp, source, page, "opentender".equals(format));
            }
            transactionUtils.commit();

            if (result != null && !result.isEmpty()) {
                hideSecretValues(result);
                populateUtils.populateBodies(result);

                if (format != null) {
                    switch (format) {
                        case "ocds":
                            return OCDSUtils.getOCDSReleasePackage(result, request.url());
                        case "ocds-arr":
                            return getOCDSPackagesArray(result, request.url());
                        case "opentender":
                            result.stream().forEach(Server::removeNonsenses);

                            return result;
                        default:
                            halt(400, "Unsupported format");
                            return null;
                    }
                }
            }
            
            return result != null && returnFirst ? result.get(0) : result;
        }, new JsonTransformer());
    }

    /**
     * Removes nonsensical values.
     * 
     * @param tender
     *      tender to be cleaned
     */
    private static void removeNonsenses(final MasterTender tender) {
        if (tender == null) {
            return;
        }

        if (tender.getLots() != null) {
            tender.getLots().stream()
                .filter(l -> l.getBidsCount() != null)
                .forEach(l -> {
                    // remove nonsensical lot bids count
                    if (l.getBidsCount() < 0 || l.getBidsCount() > 0) {
                        l.setBidsCount(null);
                    }

                    // remove nonsensical lot prices
                    if (l.getEstimatedPrice() != null) {
                        l.getEstimatedPrice().setNetAmountEur(removeNonsensicalAmount(
                            l.getEstimatedPrice().getNetAmountEur(), null, PRICE_MAX));
                    }

                    // remove nonsensical bid prices
                    if (l.getBids() != null) {
                        l.getBids().stream()
                            .filter(b -> b.getPrice() != null)
                            .forEach(b -> {
                                b.getPrice().setNetAmountEur(removeNonsensicalAmount(
                                    b.getPrice().getNetAmountEur(), null, PRICE_MAX));
                            });
                    }

                    // remove nonsensical lot dates
                    l.setEstimatedStartDate(removeNonsensicalDateTime(l.getEstimatedStartDate(),
                        DATE_MIN, DATE_MAX));
                    l.setEstimatedCompletionDate(removeNonsensicalDateTime(
                        l.getEstimatedCompletionDate(), DATE_MIN, DATE_MAX));
                    l.setAwardDecisionDate(removeNonsensicalDateTime(l.getAwardDecisionDate(),
                        DATE_MIN, DATE_MAX));
                    l.setContractSignatureDate(removeNonsensicalDateTime(
                        l.getContractSignatureDate(), DATE_MIN, DATE_MAX));
                    l.setCompletionDate(removeNonsensicalDateTime(l.getCompletionDate(),
                        DATE_MIN, DATE_MAX));
                    l.setCancellationDate(removeNonsensicalDateTime(l.getCancellationDate(),
                        DATE_MIN, DATE_MAX));
                });
        }

        // remove nonsensical publication dates
        if (tender.getPublications() != null) {
            tender.getPublications().forEach(p -> {
                p.setPublicationDate(removeNonsensicalDateTime(p.getPublicationDate(), DATE_MIN,
                    DATE_MAX));
                p.setDispatchDate(removeNonsensicalDateTime(p.getDispatchDate(), DATE_MIN,
                    DATE_MAX));
                p.setLastUpdate(removeNonsensicalDateTime(p.getLastUpdate(), DATE_MIN,
                    DATE_MAX));
            });
        }

        // remove nonsensical tender dates
        //TODO: estimatedInvitationDate, noObjectionDate ??
        tender.setBidDeadline(removeNonsensicalDateTime(tender.getBidDeadline(), DATE_MIN, DATE_MAX));
        tender.setDocumentsDeadline(removeNonsensicalDateTime(tender.getDocumentsDeadline(), DATE_MIN,
            DATE_MAX));
        tender.setEstimatedStartDate(removeNonsensicalDateTime(tender.getEstimatedStartDate(), DATE_MIN,
            DATE_MAX));
        tender.setEstimatedCompletionDate(removeNonsensicalDateTime(tender.getEstimatedCompletionDate(),
            DATE_MIN, DATE_MAX));
        tender.setAwardDecisionDate(removeNonsensicalDateTime(tender.getAwardDecisionDate(),
            DATE_MIN, DATE_MAX));
        tender.setContractSignatureDate(removeNonsensicalDateTime(tender.getContractSignatureDate(),
            DATE_MIN, DATE_MAX));
        tender.setCancellationDate(removeNonsensicalDateTime(tender.getCancellationDate(),
            DATE_MIN, DATE_MAX));
        tender.setEnquiryDeadline(removeNonsensicalDateTime(tender.getEnquiryDeadline(),
            DATE_MIN, DATE_MAX));
        tender.setAwardDeadline(removeNonsensicalDateTime(tender.getAwardDeadline(),
            DATE_MIN, DATE_MAX));

        // remove nonsensical tender prices
        if (tender.getFinalPrice() != null) {
            tender.getFinalPrice().setNetAmountEur(removeNonsensicalAmount(
                tender.getFinalPrice().getNetAmountEur(), null, PRICE_MAX));
        }
        if (tender.getEstimatedPrice() != null) {
            tender.getEstimatedPrice().setNetAmountEur(removeNonsensicalAmount(
                tender.getEstimatedPrice().getNetAmountEur(), null, PRICE_MAX));
        }
    }

    /**
     * @param result
     *      list of master tenders
     * @param uri
     *      request uri
     * @return list of OCDS release packages, each including one tender
     */
    private static List<OCDSReleasePackage> getOCDSPackagesArray(final List<MasterTender> result, final String uri) {
        if (result == null || result.isEmpty()) {
            return null;
        }
        
        URL url;
        try {
            url = new URL(uri);
        } catch (MalformedURLException ex) {
            logger.error("Request URI '{}' is malformed because of", uri, ex);
            throw new UnrecoverableException("Request URI is malformed", ex);
        }

        final String basePackageUrl = url.getProtocol() + "://" + url.getHost()
            + (url.getPort() != -1 ? ":" + url.getPort() : "")
            + "/master_tender/ocds/";

        return result.stream().map(t -> OCDSUtils.getOCDSReleasePackage(t, basePackageUrl + t.getId()))
            .collect(Collectors.toList());
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

        populateUtils = new PopulateUtils(masterBodyDao);
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
