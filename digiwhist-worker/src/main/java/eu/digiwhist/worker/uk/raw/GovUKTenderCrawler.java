package eu.digiwhist.worker.uk.raw;

import static org.apache.commons.codec.digest.DigestUtils.sha256Hex;

import java.io.IOException;
import java.time.LocalDate;
import java.time.Month;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;

import com.gargoylesoftware.htmlunit.WebClient;
import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlElement;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import com.gargoylesoftware.htmlunit.html.HtmlSubmitInput;
import com.gargoylesoftware.htmlunit.html.HtmlTextInput;
import com.gargoylesoftware.htmlunit.xml.XmlPage;

import eu.digiwhist.dataaccess.dao.DAOFactory;
import eu.digiwhist.dataaccess.dto.codetables.PublicationSources;
import eu.digiwhist.worker.raw.BaseDigiwhistIncrementalCrawler;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.RawDataDAO;
import eu.dl.dataaccess.dto.raw.Raw;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.Message;
import eu.dl.worker.MessageFactory;

/**
 * Crawler/Downloader from UK Gov Tenders. Crawler and downloader in one, because source does not have permanent links
 * to documents and documents are too big to be sent in message. Source is also not very reliable, page is reloaded
 * several times before crashing.
 *
 * @author Tomas Mrazek
 */
public final class GovUKTenderCrawler extends BaseDigiwhistIncrementalCrawler {

    private static final String VERSION = "4";
    private static final String PERSISTENT_ID_PREFIX = "UK";

    /**
     * Page with search.
     */
    private static final String FIRST_PAGE = PublicationSources.UK_GOV + "Search";

    /**
     * Date format and formatter for searching.
     */
    private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("dd/MM/yyyy");
    private static final LocalDate DEFAULT_START_DATE = LocalDate.of(2014, Month.DECEMBER, 17);

    /**
     * XPATH selectors.
     */
    private static final String DATE_FROM_INPUT_XPATH = "//input[@name='published_from']";
    private static final String DATE_TO_INPUT_XPATH = "//input[@name='published_to']";
    private static final String SEARCH_BUTTON_XPATH = "//input[@name='adv_search']";

    /**
     * Webclient to crawl with.
     */
    private final WebClient webClient;

    /**
     * HtmlPage holder for faster crawling.
     */
    private HtmlPage searchPage = null;

    /**
     * DAO to save tender to raw_data.
     */
    private RawDataDAO<RawData> rawDao;

    /**
     * Error handling.
     */
    private static final int errorCap = 50;


    /**
     * Default constructor.
     */
    public GovUKTenderCrawler() {
        webClient = new WebClient();
        webClient.getOptions().setTimeout(30000);
        webClient.getOptions().setUseInsecureSSL(true);
        webClient.getOptions().setThrowExceptionOnScriptError(false);

        // we will be saving data in crawler already,
        rawDao = DAOFactory.getDAOFactory().getRawTenderDAO(getName(), getVersion());
    }

    /**
     * Gets default start date (date of the oldest record).
     *
     * @return default start date (date of the oldest record)
     */
    public LocalDate getDefaultStartDate() {
        return DEFAULT_START_DATE;
    }

    /**
     * Returns the actual version of crawler worker.
     *
     * @return actual crawler worker version
     */
    public String getVersion() {
        return VERSION;
    }

    /**
     * Returns the actual version of crawler, static method for GovUK Crawler Resender.
     *
     * @return actual crawler worker version
     */
    public static String getStaticVersion() {
        return VERSION;
    }

    @Override
    protected void initialSetup() {
        loadSearchPage();
    }

    @Override
    protected void crawlSourceForDate(final LocalDate date) {
        int errorCounter = 0;

        Integer tenderCrawlerCounter = 0;

        // Crawler tries several times, before throwing an error.
        while (true) {
            try {
                // Get results for requested date.
                try {
                    searchPage = getResultPageForDate(searchPage, date);
                } catch (IOException e) {
                    logger.error("Cannot get result page for {}", date);
                    throw new IOException("Cannot get result page for", e);
                }
                logger.debug("Result page for {} retrieved.", date);

                // Download results as XML
                final XmlPage xmlDocument;
                final HtmlAnchor downloadXmlButton =
                        searchPage.getFirstByXPath("//a[contains(text(),'Download as XML')]");
                try {
                    xmlDocument = downloadXmlButton.click();
                } catch (IOException e) {
                    logger.error("Unable to download XML for for {}", date);
                    throw new IOException("Unable to download XML for for", e);
                }
                logger.debug("XML document for {} downloaded.", date);

                // Save to raw data.
                final String messageId = savePageAndGetRecordId(xmlDocument, date, tenderCrawlerCounter);

                // Publish message.
                final Message outgoingMessage = MessageFactory.getMessage();
                outgoingMessage.setValue("id", messageId);
                publishMessage(outgoingMessage);
            } catch (IOException e) {
                if (errorCounter++ < errorCap) {
                    logger.warn("Error on crawling date {} trying again.");
                    continue;
                } else {
                    logger.error("Unable to crawl date {}", date);
                    throw new UnrecoverableException("Unable to crawl because ", e);
                }
            }

            return;
        }
    }

    @Override
    protected void finalCleanup() {
        // erase temporary search page
        searchPage = null;
    }

    @Override
    protected ChronoUnit getIncrementUnit() {
        return ChronoUnit.DAYS;
    }

    /**
     * Load search page into searchPage.
     */
    private void loadSearchPage() {
        // Open search page, this will be used for all dates.
        try {
            final HtmlPage firstPage = webClient.getPage(FIRST_PAGE);
            final HtmlSubmitInput searchButton = firstPage.getFirstByXPath("//input[@name='adv_search']");
            searchPage = searchButton.click();
        } catch (IOException e) {
            logger.error("Cannot open search page for {}", e);
            throw new UnrecoverableException("Cannot open search page", e);
        }
        logger.debug("Search page loaded.");
    }

    /**
     * Save page to raw data and return its id.
     *
     * @param page page to save
     * @param date publish date of tender
     * @param counter number of tender crawled on published day
     *
     * @return record id
     */
    private String savePageAndGetRecordId(final XmlPage page, final LocalDate date, final Integer counter) {
        final RawData rawData = new RawData();

        rawData.setSourceUrl(page.getUrl());
        rawData.setSourceData(page.getWebResponse().getContentAsString());

        getTransactionUtils().begin();
        rawData.setPersistentId(generatePersistentId(rawData, PERSISTENT_ID_PREFIX, date, counter));
        final String savedId = rawDao.save(rawData);
        getTransactionUtils().commit();
        logger.info("Stored raw data as {}", savedId);

        return savedId;
    }

    /**
     * Get page with results for date.
     *
     * @param page search page
     * @param date date to search for
     *
     * @return HtmlPage
     * @throws IOException exception
     */
    private HtmlPage getResultPageForDate(final HtmlPage page, final LocalDate date) throws IOException {
        final String localDateString = date.format(DATE_FORMATTER);

        final HtmlTextInput dateFromInput = page.getFirstByXPath(DATE_FROM_INPUT_XPATH);
        dateFromInput.setText(localDateString);

        final HtmlTextInput dateToInput = page.getFirstByXPath(DATE_TO_INPUT_XPATH);
        dateToInput.setText(localDateString);

        final HtmlElement searchButton = page.getFirstByXPath(SEARCH_BUTTON_XPATH);
        return searchButton.click();
    }

    /**
     * Generates persistent id in the form of PREFIX_md5(sourceUrl).
     *
     * @param raw persistent id is generated for this item
     * @param persistentIdPrefix persistent id prefix
     * @param date publish date of tender
     * @param counter number of tender crawled on published day
     *
     * @return persistent id or null in case there is no source url set
     */
    public static String generatePersistentId(final Raw raw, final String persistentIdPrefix,
                                              final LocalDate date, final Integer counter) {
        String builder = null;
        if (raw.getSourceUrl() != null) {
            builder = raw.getSourceUrl().toString();
            if (raw.getSourceFileName() != null) {
                builder = builder + raw.getSourceFileName();
            }

            if (date != null && counter != null) {
                builder = builder + date + counter;
            }
        }

        if (builder != null) {
            return persistentIdPrefix + "_" + sha256Hex(builder);
        } else {
            return null;
        }
    };
}
