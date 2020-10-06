package eu.datlab.server;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.dataaccess.dao.CleanTenderDAO;
import eu.dl.dataaccess.dao.MasterBodyDAO;
import eu.dl.dataaccess.dao.MasterTenderApiDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.clean.CleanTender;
import eu.dl.dataaccess.dto.master.MasterBid;
import eu.dl.dataaccess.dto.master.MasterBody;
import eu.dl.dataaccess.dto.master.MasterTender;
import eu.dl.dataaccess.dto.master.MasterTenderLot;
import eu.dl.dataaccess.dto.utils.OCDSUtils;
import eu.dl.dataaccess.utils.PopulateUtils;
import eu.dl.server.BaseServer;
import eu.dl.server.JsonTransformer;
import eu.dl.server.exceptions.NotFoundException;
import eu.dl.server.exceptions.ParameterFormattingException;
import org.json.JSONObject;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Month;
import java.util.List;
import java.util.stream.Collectors;

import static eu.dl.dataaccess.utils.RemoveNonsenseUtils.removeNonsensicalAmount;
import static eu.dl.dataaccess.utils.RemoveNonsenseUtils.removeNonsensicalDateTime;
import static spark.Spark.get;

/**
 * Main class for API.
 *
 * @author Kuba Krafka
 */
public final class TenderApi extends BaseServer {

    private static final String VERSION = "2";

    private static final String NAME = "TenderApi";

    private TransactionUtils transactionUtils;

    private CleanTenderDAO cleanDao;

    private MasterTenderApiDAO masterDao;

    private MasterBodyDAO masterBodyDao;

    private PopulateUtils populateUtils;

    /**
     * Default output format.
     */
    private static final String DEFAULT_FORMAT = "json";

    /**
     * Default decision, whether returns only open tenders.
     */
    private static final String DEFAULT_IS_OPENTENDER = "false";

    /**
     * OCDS max price. Bigger amounts are set to NULL for 'ocds' format.
     */
    private static final BigDecimal PRICE_MAX = BigDecimal.valueOf(1200000000);
    /**
     * OCDS min date. Earlier dates are set to NULL for 'ocds' format.
     */
    private static final LocalDate DATE_MIN = LocalDate.of(2000, Month.JANUARY, 1);
    /**
     * OCDS max date. Later dates are set to NULL for 'ocds' format.
     */
    private static final LocalDate DATE_MAX = LocalDate.of(2025, Month.JANUARY, 1);

    @Override
    public void start() {
        init();

        registerMasterTenderEndpoints();

        registerCleanTenderEndpoints();

        registerMasterBodyEndpoints();

    }


    /**
     * Registers endpoints for clean tender API.
     */
    private void registerCleanTenderEndpoints() {
        get("/protected/clean_tender", "application/json", (request, response) -> {
            LocalDateTime timestamp = getDate(request.queryParams("timestamp"));
            Integer page = getInteger(request.queryParams("page"));
            String country = request.queryParams("country");
            String source = request.queryParams("source");

            transactionUtils.begin();
            List<CleanTender> result = cleanDao.getModifiedAfter(timestamp, source, country, page);
            transactionUtils.commit();
            return result;
        }, new JsonTransformer());

        get("/protected/clean_tender/id/:id", "application/json", (request, response) -> {
            String id = request.params("id");
            transactionUtils.begin();
            CleanTender result = cleanDao.getById(id);
            if (result == null) {
                throw new NotFoundException("No such tender found. Id: " + id);
            }
            transactionUtils.commit();
            return result;
        }, new JsonTransformer());
    }

    /**
     * Registers endpoints for master tender API.
     */
    private void registerMasterTenderEndpoints() {
        get("/protected/master_tender", "application/json", (request, response) -> {
            LocalDateTime timestamp = getDate(request.queryParams("timestamp"));
            Integer page = getInteger(request.queryParams("page"));
            String country = request.queryParams("country");
            String source = request.queryParams("source");
            String format = request.queryParamOrDefault("format", DEFAULT_FORMAT).toLowerCase();
            Boolean opentender = Boolean.valueOf(request.queryParamOrDefault("opentender", DEFAULT_IS_OPENTENDER));
            String pageSize = request.queryParams("page_size");

            transactionUtils.begin();

            List<MasterTender> result;
            if (pageSize != null) {
                result = masterDao.getModifiedAfter(timestamp, source, country, page, opentender, getInteger(pageSize));
            } else {
                result = masterDao.getModifiedAfter(timestamp, source, country, page, opentender);
            }

            populateUtils.populateBodies(result);
            transactionUtils.commit();

            if (format != null && !format.isEmpty()) {
                switch (format) {
                    case "json":
                        // do nothing
                        return result;
                    case "ocds":
                        final String basePackageUrl = request.url() + "/id/%s?format=ocds";

                        return result.stream()
                            .map(TenderApi::hideSecretValues)
                            .map(TenderApi::removeNonsenses)
                            .map(t -> OCDSUtils.getOCDSReleasePackage(t, String.format(basePackageUrl, t.getId())))
                            .collect(Collectors.toList());
                    default:
                        logger.error("Format '{}' is unsupported.", format);
                        throw new ParameterFormattingException("Unsupported format");
                }
            }

            return result;
        }, new JsonTransformer());

        get("/protected/master_tender/id/:id", "application/json", (request, response) -> {
            String id = request.params("id");
            String format = request.queryParamOrDefault("format", DEFAULT_FORMAT).toLowerCase();

            transactionUtils.begin();
            MasterTender result = masterDao.getById(id);
            if (result == null) {
                throw new NotFoundException("No such tender found. Id: " + id);
            }
            populateUtils.populateBodies(result);
            transactionUtils.commit();

            if (format != null && !format.isEmpty()) {
                switch (format) {
                    case "json":
                        // do nothing
                        return result;
                    case "ocds":
                        return OCDSUtils.getOCDSReleasePackage(removeNonsenses(hideSecretValues(result)), request.url() + "?format=ocds");
                    default:
                        logger.error("Format '{}' is unsupported.", format);
                        throw new ParameterFormattingException("Unsupported format");
                }
            }

            return result;
        }, new JsonTransformer());

        get("/protected/master_tender_count", "application/json", (request, response) -> {
            LocalDateTime timestamp = getDate(request.queryParams("timestamp"));
            String country = request.queryParams("country");
            String source = request.queryParams("source");

            transactionUtils.begin();

            Integer count = masterDao.getModifiedAfterCount(timestamp, source, country);
            JSONObject result = new JSONObject();

            result.put("count", count);
            transactionUtils.commit();
            return result;
        }, new JsonTransformer());

        get("/protected/master_tender/buyer_profile_matching", "application/json", (request, response) -> {
            LocalDateTime timestamp = getDate(request.queryParams("timestamp"));
            Integer page = getInteger(request.queryParams("page"));
            String source = request.queryParams("source");
            String pageSize = request.queryParams("page_size");

            transactionUtils.begin();

            List<MasterTender> result = masterDao.getModifiedAfterForBuyerProfileMatching(timestamp, source, page, getInteger(pageSize));

            populateUtils.populateBodies(result);
            transactionUtils.commit();

            return result;
        }, new JsonTransformer());
    }

    /**
     * Registers endpoints for master body API.
     */
    private void registerMasterBodyEndpoints() {
        get("/protected/master_body/id/:id", "application/json", (request, response) -> {
            String id = request.params("id");
            transactionUtils.begin();
            MasterBody result = masterBodyDao.getById(id);
            if (result == null) {
                throw new NotFoundException("No such body found. Id: " + id);
            }
            transactionUtils.commit();
            return result;
        }, new JsonTransformer());
    }

    /**
     * Inits DAOs and similar.
     */
    private void init() {
        transactionUtils = DAOFactory.getDAOFactory().getTransactionUtils();

        cleanDao = DAOFactory.getDAOFactory().getCleanTenderDAO(NAME, VERSION);

        masterDao = (MasterTenderApiDAO) DAOFactory.getDAOFactory().getMasterTenderDAO(NAME, VERSION);

        masterBodyDao = DAOFactory.getDAOFactory().getMasterBodyDAO(NAME, VERSION);

        populateUtils = new PopulateUtils(masterBodyDao);
    }

    /**
     * Removes nonsensical values.
     *
     * @param tender
     *      tender to be cleaned
     * @return cleaned tender
     */
    private static MasterTender removeNonsenses(final MasterTender tender) {
        if (tender == null) {
            return null;
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
                        l.getEstimatedPrice().setNetAmountEur(removeNonsensicalAmount(l.getEstimatedPrice().getNetAmountEur(), null,
                            PRICE_MAX));
                    }

                    // remove nonsensical bid prices
                    if (l.getBids() != null) {
                        l.getBids().stream()
                            .filter(b -> b.getPrice() != null)
                            .forEach(b -> b.getPrice().setNetAmountEur(removeNonsensicalAmount(b.getPrice().getNetAmountEur(), null,
                                PRICE_MAX)));
                    }

                    // remove nonsensical lot dates
                    l.setEstimatedStartDate(removeNonsensicalDateTime(l.getEstimatedStartDate(), DATE_MIN, DATE_MAX));
                    l.setEstimatedCompletionDate(removeNonsensicalDateTime(l.getEstimatedCompletionDate(), DATE_MIN, DATE_MAX));
                    l.setAwardDecisionDate(removeNonsensicalDateTime(l.getAwardDecisionDate(), DATE_MIN, DATE_MAX));
                    l.setContractSignatureDate(removeNonsensicalDateTime(l.getContractSignatureDate(), DATE_MIN, DATE_MAX));
                    l.setCompletionDate(removeNonsensicalDateTime(l.getCompletionDate(), DATE_MIN, DATE_MAX));
                    l.setCancellationDate(removeNonsensicalDateTime(l.getCancellationDate(), DATE_MIN, DATE_MAX));
                });
        }

        // remove nonsensical publication dates
        if (tender.getPublications() != null) {
            tender.getPublications().forEach(p -> {
                p.setPublicationDate(removeNonsensicalDateTime(p.getPublicationDate(), DATE_MIN, DATE_MAX));
                p.setDispatchDate(removeNonsensicalDateTime(p.getDispatchDate(), DATE_MIN, DATE_MAX));
                p.setLastUpdate(removeNonsensicalDateTime(p.getLastUpdate(), DATE_MIN, DATE_MAX));
            });
        }

        // remove nonsensical tender dates
        //TODO: estimatedInvitationDate, noObjectionDate ??
        tender.setBidDeadline(removeNonsensicalDateTime(tender.getBidDeadline(), DATE_MIN, DATE_MAX));
        tender.setDocumentsDeadline(removeNonsensicalDateTime(tender.getDocumentsDeadline(), DATE_MIN, DATE_MAX));
        tender.setEstimatedStartDate(removeNonsensicalDateTime(tender.getEstimatedStartDate(), DATE_MIN, DATE_MAX));
        tender.setEstimatedCompletionDate(removeNonsensicalDateTime(tender.getEstimatedCompletionDate(), DATE_MIN, DATE_MAX));
        tender.setAwardDecisionDate(removeNonsensicalDateTime(tender.getAwardDecisionDate(), DATE_MIN, DATE_MAX));
        tender.setContractSignatureDate(removeNonsensicalDateTime(tender.getContractSignatureDate(), DATE_MIN, DATE_MAX));
        tender.setCancellationDate(removeNonsensicalDateTime(tender.getCancellationDate(), DATE_MIN, DATE_MAX));
        tender.setEnquiryDeadline(removeNonsensicalDateTime(tender.getEnquiryDeadline(), DATE_MIN, DATE_MAX));
        tender.setAwardDeadline(removeNonsensicalDateTime(tender.getAwardDeadline(), DATE_MIN, DATE_MAX));

        // remove nonsensical tender prices
        if (tender.getFinalPrice() != null) {
            tender.getFinalPrice().setNetAmountEur(removeNonsensicalAmount(tender.getFinalPrice().getNetAmountEur(), null, PRICE_MAX));
        }
        if (tender.getEstimatedPrice() != null) {
            tender.getEstimatedPrice().setNetAmountEur(removeNonsensicalAmount(tender.getEstimatedPrice().getNetAmountEur(), null,
                PRICE_MAX));
        }

        return tender;
    }

    /**
     * This methods hides specific values from public API.
     *
     * @param tender
     *      tender to be processed
     * @return updated tender
     */
    private static MasterTender hideSecretValues(final MasterTender tender) {
        if (tender == null) {
            return null;
        }

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

        return tender;
    }
}
