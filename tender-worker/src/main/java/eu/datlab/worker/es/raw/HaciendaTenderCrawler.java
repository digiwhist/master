package eu.datlab.worker.es.raw;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.dataaccess.dao.CrawlerAuditDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.raw.CrawlerAuditRecord;
import eu.dl.worker.Message;
import eu.dl.worker.raw.BaseCrawler;

import java.time.LocalDate;
import java.time.Month;
import java.util.HashMap;

/**
 * Crawls Spanish data from Hacienda source.
 */
public final class HaciendaTenderCrawler extends BaseCrawler {
    private static final String VERSION = "1.0";

    // base URLs for three groups from source page
    private static final String BASE_JOURNAL_URL =
            "https://contrataciondelestado.es/sindicacion/sindicacion_643/licitacionesPerfilesContratanteCompleto3_%d.zip";

    private static final String BASE_AGGREGATION_URL =
            "https://contrataciondelestado.es/sindicacion/sindicacion_1044/PlataformasAgregadasSinMenores_%d.zip";

    private static final String BASE_SMALL_URL =
            "https://contrataciondelestado.es/sindicacion/sindicacion_1143/contratosMenoresPerfilesContratantes_%d.zip";

    private static final Integer JOURNAL_DEFAULT_START_YEAR = 2012;
    private static final Integer AGGREGATION_DEFAULT_START_YEAR = 2016;
    private static final Integer SMALL_DEFAULT_START_YEAR = 2018;

    private static final LocalDate DEFAULT_START_DATE_BY_MONTH = LocalDate.of(2019, Month.JANUARY, 1);

    private final CrawlerAuditDAO<CrawlerAuditRecord> crawlerAuditDao;
    private final CrawlerAuditRecord crawlerAuditRecord;
    private HashMap<String, Object> auditMetaData;
    private LocalDate lastCrawledDate;

    /**
     * Initialise crawler audit DAO.
     */
    public HaciendaTenderCrawler() {
        crawlerAuditDao = DAOFactory.getDAOFactory().getCrawlerAuditDAO(getName(), getVersion());
        crawlerAuditDao.setWorkerName(getName());
        crawlerAuditRecord = crawlerAuditDao.getByNameAndVersion();
        auditMetaData = crawlerAuditRecord.getMetaData();
        if (auditMetaData == null) {
            auditMetaData = new HashMap<>();
        }
    }

    @Override
    protected String getVersion() {
        return VERSION;
    }


    /**
     * Returns date value of object.
     *
     * @param object object to get date value from
     * @return date value
     */
    private LocalDate getDateValue(final Object object) {
        if (object == null) {
            return null;
        }
        return LocalDate.parse(object.toString());
    }

    /**
     * Proceeds all tenders from one group which were published by years.
     *
     * @param group identifier of group
     */
    private void proceedTendersByYear(final String group) {
        if (group == null || group.isEmpty()) {
            return;
        }
        lastCrawledDate = getDateValue(auditMetaData.get("lastCrawledDate" + group));
        // getting start year based on group
        int year = group.equals("JOURNAL") ? JOURNAL_DEFAULT_START_YEAR
                : (group.equals("AGGREGATION") ? AGGREGATION_DEFAULT_START_YEAR : SMALL_DEFAULT_START_YEAR);
        // year is start year or the next year after last crawled date (the latest one)
        if (lastCrawledDate != null) {
            year = Math.max(year, lastCrawledDate.getYear() + 1);
        }
        // here we proceed only tenders before DEFAULT_START_DATE_BY_MONTH,
        // after that date tenders were published by month
        while (year < DEFAULT_START_DATE_BY_MONTH.getYear()) {
            String url = String.format((group.equals("JOURNAL") ? BASE_JOURNAL_URL
                    : (group.equals("AGGREGATION") ? BASE_AGGREGATION_URL : BASE_SMALL_URL)), year);
            final HashMap<String, Object> metaData = new HashMap<>();
            metaData.put("year", year);
            createAndPublishMessage(url, metaData);
            if (auditMetaData.containsKey("lastCrawledDate" + group)) {
                auditMetaData.replace("lastCrawledDate" + group, LocalDate.ofYearDay(year, 1).toString());
            } else {
                auditMetaData.put("lastCrawledDate" + group, LocalDate.ofYearDay(year, 1).toString());
            }
            crawlerAuditRecord.setMetaData(auditMetaData);
            crawlerAuditDao.save(crawlerAuditRecord);
            year++;
        }
    }

    /**
     * Proceeds all tenders from one group which were published by month.
     *
     * @param group identifier of group
     */
    private void proceedTendersByMonth(final String group) {
        lastCrawledDate = getDateValue(auditMetaData.get("lastCrawledDate" + group));

        LocalDate actualDate = DEFAULT_START_DATE_BY_MONTH;
        // actualDate default start date or the next date after last crawled date (the latest one)
        if (lastCrawledDate != null) {
            actualDate = (lastCrawledDate.withDayOfMonth(1).plusMonths(1)).compareTo(DEFAULT_START_DATE_BY_MONTH.withDayOfMonth(1)) > 0
                    ? lastCrawledDate.withDayOfMonth(1).plusMonths(1) : DEFAULT_START_DATE_BY_MONTH;
        }
        LocalDate now = LocalDate.now();

        // wee proceed tenders, published before actual month (actual month's tenders will be published the next month)
        while (actualDate.getYear() < now.getYear()
                || (actualDate.getYear() == now.getYear() && actualDate.getMonth().compareTo(now.getMonth()) < 0)) {
            // getting base url based on group
            String url = String.format((group.equals("JOURNAL") ? BASE_JOURNAL_URL
                            : (group.equals("AGGREGATION") ? BASE_AGGREGATION_URL : BASE_SMALL_URL)),
                    Integer.parseInt(actualDate.getYear() + String.format("%02d", actualDate.getMonth().getValue())));
            final HashMap<String, Object> metaData = new HashMap<>();
            metaData.put("date", actualDate);
            createAndPublishMessage(url, metaData);


            if (auditMetaData.containsKey("lastCrawledDate" + group)) {
                auditMetaData.replace("lastCrawledDate" + group, actualDate.toString());
            } else {
                auditMetaData.put("lastCrawledDate" + group, actualDate.toString());
            }
            crawlerAuditRecord.setMetaData(auditMetaData);
            crawlerAuditDao.save(crawlerAuditRecord);
            actualDate = actualDate.plusMonths(1);
        }
    }


    @Override
    protected void doWork(final Message message) {
        logger.info("Tenders crawling starts");

        // processing of older tenders for every group from source page
        proceedTendersByYear("JOURNAL");
        proceedTendersByYear("AGGREGATION");
        proceedTendersByYear("SMALL");

        logger.info("Tenders crawling by years finished");
        // processing of newer tenders for every group from source page
        proceedTendersByMonth("JOURNAL");
        proceedTendersByMonth("AGGREGATION");
        proceedTendersByMonth("SMALL");

        logger.info("Tenders crawling by months finished");
    }

    @Override
    protected TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }
}
