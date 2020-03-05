package eu.datlab.worker.id.raw;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gargoylesoftware.htmlunit.FailingHttpStatusCodeException;
import com.gargoylesoftware.htmlunit.Page;
import com.gargoylesoftware.htmlunit.UnexpectedPage;
import com.gargoylesoftware.htmlunit.WebClient;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.CrawlerAuditDAO;
import eu.dl.dataaccess.dao.RawDataDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.raw.BasicCrawlerAuditRecord;
import eu.dl.dataaccess.dto.raw.CrawlerAuditRecord;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.Message;
import eu.dl.worker.clean.utils.StringUtils;
import eu.dl.worker.clean.utils.URLSchemeType;
import eu.dl.worker.raw.BaseCrawler;

import java.io.IOException;
import java.net.URL;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * LPSE subject's tenders crawler in Indonesia.
 *
 * @author Tomas Mrazek
 */
public final class LPSETenderCrawler extends BaseCrawler {
    private static final String VERSION = "1.0";

    private final RawDataDAO<RawData> sourceDAO = DAOFactory.getDAOFactory().getRawTenderDAO(LPSEOrganizationDownloader.class.getName(),
        LPSEOrganizationDownloader.VERSION);

    private static final String MAIN_TAB_URL = "%1$s/eproc4/lelang/%2$s/pengumumanlelang";
    private static final String PESERTA_TAB_URL = "%1$s/eproc4/lelang/%2$s/peserta";
    private static final String HASIL_TAB_URL = "%1$s/eproc4/evaluasi/%2$s/hasil";
    private static final String PEMANANG_TAB_URL = "%1$s/eproc4/evaluasi/%2$s/pemenang";
    private static final String BERKONTRAK_TAB_URL = "%1$s/eproc4/evaluasi/%2$s/pemenangberkontrak";
    private static final String JADWAL_LINK_URL = "%1$s/eproc4/lelang/%2$s/jadwal";

    private static final String NTH_PAGE_URL = "%1$s/eproc4/dt/lelang?start=%2$d&length=%3$d";

    private static final int CONNECTION_TIMEOUT = 120000;

    private String currentSourceUrl;

    private final CrawlerAuditDAO<CrawlerAuditRecord> auditDAO = DAOFactory.getDAOFactory().getCrawlerAuditDAO(getName(), getVersion());

    private static final Integer PAGE_SIZE = 1000;

    private WebClient webClient;

    private ObjectMapper mapper;

    /**
     * String used as User-agent header for HTTP requests.
     */
    private static final String USER_AGENT = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_6) AppleWebKit/537.36 (KHTML, like Gecko)" +
        " Chrome/68.0.3440.84 Safari/537.36";


    /**
     * Web client initialization.
     */
    public LPSETenderCrawler() {
        super();

        webClient = new WebClient();
        webClient.getOptions().setJavaScriptEnabled(true);
        webClient.getOptions().setCssEnabled(false);
        webClient.getOptions().setUseInsecureSSL(true);
        webClient.getOptions().setThrowExceptionOnScriptError(false);
        webClient.getOptions().setThrowExceptionOnFailingStatusCode(false);
        webClient.getOptions().setTimeout(CONNECTION_TIMEOUT);

        webClient.addRequestHeader("User-Agent", USER_AGENT);
        webClient.waitForBackgroundJavaScript(10000);

        mapper = new ObjectMapper();
    }

    @Override
    protected void doWork(final Message message) {
        String id = message.getValue("id");
        RawData raw = sourceDAO.getById(id);
        if (raw == null) {
            logger.error("Raw record {} doesn't exists", id);
            throw new UnrecoverableException("Raw record doesn't exist");
        }

        String sourceUrl = getBulletinUrl(raw);

        CrawlerAuditRecord audit = initCrawlerAuditRecord();

        // last crawled page for the  given organization
        Map<String, Integer> lastPages = (Map<String, Integer>) audit.getMetaData().get("lastCrawledPageNumbers");
        Integer pageNumber = lastPages.get(id);
        if (pageNumber != null) {
            logger.info("Last page number {} for organization {} found in crawler audit.", pageNumber, id);
            pageNumber += 1;
        } else {
            pageNumber = 1;
        }

        // removes /eproc or /eproc4 and/or / from the end of url
        currentSourceUrl = sourceUrl.replaceAll("(/eproc4?)?/?$", "");

        String url = currentSourceUrl + "/eproc4/lelang";

        logger.info("Crawling of {} starts", url);

        try {
            int rowsCount;
            do {
                logger.info("Processing page {}# with url {}", pageNumber, getNthPageUrl(pageNumber));

                rowsCount = 0;

                // loads JSON of n-th page
                JsonNode json = mapper.readTree(getNthPage(pageNumber));
                Iterator<JsonNode> rows = json.path("data").elements();
                while (rows.hasNext()) {
                    String tenderId = rows.next().path(0).textValue();

                    HashMap<String, Object> metaData = new HashMap<>();
                    metaData.put("additionalUrls", Arrays.asList(
                        String.format(PESERTA_TAB_URL, currentSourceUrl, tenderId),
                        String.format(PEMANANG_TAB_URL, currentSourceUrl, tenderId),
                        String.format(HASIL_TAB_URL, currentSourceUrl, tenderId),
                        String.format(BERKONTRAK_TAB_URL, currentSourceUrl, tenderId),
                        String.format(JADWAL_LINK_URL, currentSourceUrl, tenderId)
                    ));

                    createAndPublishMessage(String.format(MAIN_TAB_URL, currentSourceUrl, tenderId), metaData);

                    rowsCount += 1;
                }

                lastPages.put(id, pageNumber);
                auditDAO.save(audit);

                pageNumber += 1;
            } while (rowsCount == PAGE_SIZE);
        } catch (Exception ex) {
            logger.error("Unable to crawl page {} because of", pageNumber, ex);
            throw new UnrecoverableException("Unable to get crawl page", ex);
        }
    }

    /**
     * @param raw
     *      raw record
     * @return bulletin url
     * @throws UnrecoverableException
     *      in case that the url is not found in meta-data of raw record or is malformed
     */
    private String getBulletinUrl(final RawData raw) {
        URL sourceUrl;
        if (raw.getMetaData().containsKey("url")) {
            // attempts to clean URL
            sourceUrl = StringUtils.cleanURL((String) raw.getMetaData().get("url"), URLSchemeType.HTTP);
            if (sourceUrl == null) {
                logger.info("URL {} found in meta-data of raw record {} is malformed", raw.getId());
                throw new UnrecoverableException("URL is malformed");
            }
        } else {
            logger.info("URL not found in meta-data of raw record {}", raw.getId());
            throw new UnrecoverableException("URL not found");
        }

        return sourceUrl.toString();
    }

    /**
     * @return crawler audit record with initialized meta-data
     */
    private CrawlerAuditRecord initCrawlerAuditRecord() {
        CrawlerAuditRecord audit = auditDAO.getByNameAndVersion();
        if (audit == null) {
            audit = new BasicCrawlerAuditRecord();
        }
        if (audit.getMetaData() == null) {
            audit.setMetaData(new HashMap<>());
        }
        if (!audit.getMetaData().containsKey("lastCrawledPageNumbers")) {
            audit.getMetaData().put("lastCrawledPageNumbers", new HashMap<>());
        }

        return audit;
    }

    /**
     * @param n
     *      number of required page
     * @return n-th page JSON response body
     * @throws IOException
     *      in case that the page loading fails
     */
    private String getNthPage(final int n) throws IOException {
        Page page = webClient.getPage(getNthPageUrl(n));

        // If the HTML page is returned, attempts to execute javascript and load given page again.
        // The mentioned script does some kind of magic, that set anonymous user for the session. After this step is necessary to load
        // the required page again.
        if (page.isHtmlPage()) {
            if (page.getWebResponse().getStatusCode() == 403) {
                ((HtmlPage) page).executeJavaScript("_client.start();");
                page = webClient.getPage(getNthPageUrl(n));
            }
        }

        // The expected result is an instance of UnexpectedPage class (JSON page is interpreted as unexpected), for other pages throw an
        // HTTP status exception.
        // Note: Response is the instance of either UnexpectedPage (in case of JSON, that we want) or HtmlPage (in case of HTTP status
        // exception)
        if (!(page instanceof UnexpectedPage)) {
            throw new FailingHttpStatusCodeException(page.getWebResponse());
        }

        return page.getWebResponse().getContentAsString();
    }

    /**
     * @param n
     *      number of required page
     * @return url of n-th page
     */
    private String getNthPageUrl(final int n) {
        return String.format(NTH_PAGE_URL, currentSourceUrl, PAGE_SIZE * (n - 1), PAGE_SIZE);
    }

    @Override
    protected TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }

    @Override
    protected String getVersion() {
        return VERSION;
    }
}
