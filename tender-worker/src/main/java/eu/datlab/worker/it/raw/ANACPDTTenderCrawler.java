package eu.datlab.worker.it.raw;

import com.gargoylesoftware.htmlunit.ScriptException;
import com.gargoylesoftware.htmlunit.WebClient;
import com.gargoylesoftware.htmlunit.html.DomElement;
import com.gargoylesoftware.htmlunit.html.HTMLParserListener;
import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import com.gargoylesoftware.htmlunit.html.HtmlTable;
import com.gargoylesoftware.htmlunit.html.HtmlTableRow;
import com.gargoylesoftware.htmlunit.javascript.JavaScriptErrorListener;
import eu.datlab.worker.raw.BaseDatlabIncrementalCrawler;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.codetables.TenderSupplyType;
import org.apache.commons.logging.LogFactory;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.time.LocalDate;
import java.time.Month;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.logging.Level;

/**
 * Searches the "Portale della trasparenza" (http://http://portaletrasparenza.avcp.it) for tender HTML details.
 *
 * @author Tomas Mrazek
 */
public final class ANACPDTTenderCrawler extends BaseDatlabIncrementalCrawler {
    private static final String VERSION = "1.0";

    private static final String SOURCE_DOMAIN =
        "http://portaletrasparenza.avcp.it/microstrategy/html/index.htm";

    private static final String DATA_URL = "http://portaletrasparenza.anticorruzione.it/Microstrategy/asp/Main.aspx"
        + "?evt=2048001&src=Main.aspx.2048001&visMode=0&hiddenSections=header,footer,path,dockTop"
        + "&documentID=E1BCDDED4DB8F448D8B0349F96D12254"
        + "&valuePromptAnswers=-100^%1$s^-100^%4$d^-100^-100^-100^-100^%2$s^%2$s"
            + "^01/01/1800^01/01/1800^01/01/1800^01/01/1800^-100^-100^-100^-100^-100^-100^-100^%3$d"
        + "&currentViewMedia=2&Main.aspx=-10*.119*.128*.95.SISk*_Extranet.0_&shared=*-1.*-1.0.0.0"
        + "&ftb=0.422541B24E28B69DC5DF858B20E67091.*0.8.0.0-8.18_268453447.*-1.1.*0"
        + "&fb=0.422541B24E28B69DC5DF858B20E67091.Extranet.8.0.0-8.768.769.774.770.773.772.775.55.256.10.257.776"
            + ".777_268453447.*-1.1.*0&uid=web&pwd=no";

    private static final DateTimeFormatter DATA_URL_DATE_FORMATTER = DateTimeFormatter.ofPattern("dd/MM/uuuu");

    /**
     * Set of trigrams that covers all subjects names.
     */
    private static final List<String> CONTRACTING_AUTHORITY_TRIGRAMS = Arrays.asList("i b", "i r", "oci", "lla", "nal",
        "inc", "ian", "e s", "erv", ".a.", "i c", "spe", "one", "i s", "a u", "nit", "spa", "i m", "i p", "i t", "ale",
        "i a", "i f", "une", "del", "ist", ".l.", "srl", "azi", "a d", "ent", ".p.", "ein", "con", "lic", "gen", "ano",
        "asl", "per", "ita", "ttu", "com", "mbh", "ter", "ola", "mar", ".c.", "ung", "col", "sic", "asp", "lia", "cer",
        "ato", "aut", "orm", "uni", "r l", "olo", "ell", "i g", "chi", "ria", "nza", "eri", "ene", "ran", "ino", "tro",
        ".i.", "ber", "e-a", "esc", "a s", "ver", ".s.", "mer", "ili", "tpa", "ard", "sca", "olf", "lat", "ant", "ser",
        "pab", "ori", "gli", "usa", "tal", "sfe", "san", "rom", "par", "ont", "onl", "ida", "dic", "u n", "tra", "tan",
        "sch", "o s", "fer", "ass", "zzo", "zon", "uid", "tta", "tre", "tic", "stu", "ssa", "sis", "sio", "sei", "s.e",
        ".sa", "rno", "oop", "oga", "nno", "miu", "map", "lca");
    
    /**
     * Maximal count of the returned records.
     */
    private static final int RECORDS_MAX_COUNT = 300;

    private static final int HTTP_SUCCESS_CODE = 200;

    private static final String RESULTS_TABLE_ID = "table_grid_K33_0";

    private static final LocalDate DEFAULT_DATE = LocalDate.of(2015, Month.JANUARY, 1);

    /**
     * Value for GET filter paramter which is not set.
     */
    private static final int NONFILTER_VALUE = -100;

    private WebClient webClient;

    /**
     * Time limit in milliseconds for loading of the n-th result page.
     */
    private static final int RESULTSET_PAGE_TIMEOUT = 60000;
    /**
     * Silences all warning/error handlers and registrated loggers of the given {@code webClient}.
     *
     * @param client
     *      web client being to silencing
     */
    private static void silenceWebClient(final WebClient client) {
        LogFactory.getFactory()
            .setAttribute("org.apache.commons.logging.Log", "org.apache.commons.logging.impl.NoOpLog");
        java.util.logging.Logger.getLogger("com.gargoylesoftware.htmlunit").setLevel(Level.OFF);
        java.util.logging.Logger.getLogger("org.apache.commons.httpclient").setLevel(Level.OFF);
        
        client.setIncorrectnessListener((final String arg0, final Object arg1) -> {
        });

        client.setJavaScriptErrorListener(new JavaScriptErrorListener() {
            @Override
            public void scriptException(final HtmlPage page, final ScriptException scriptException) {
            }
            @Override
            public void timeoutError(final HtmlPage page, final long allowedTime, final long executionTime) {
            }
            @Override
            public void malformedScriptURL(final HtmlPage page, final String url,
                final MalformedURLException malformedURLException) {
            }
            @Override
            public void loadScriptError(final HtmlPage page, final URL scriptUrl, final Exception exception) {
            }

            @Override
            public void warn(final String s, final String s1, final int i, final String s2, final int i1) {
            }
        });
        client.setHTMLParserListener(new HTMLParserListener() {
            @Override
            public void error(final String message, final URL url, final String html, final int line, final int column,
                final String key) {
            }
            @Override
            public void warning(final String message, final URL url, final String html, final int line,
                final int column, final String key) {
            }
        });
    }

    @Override
    protected void crawlSourceForDate(final LocalDate date) {
        logger.info("Crawling for date '{}' starts", date);

        for (String trigram : CONTRACTING_AUTHORITY_TRIGRAMS) {
            //crawling of active tenders
            crawlSourceForCriteria(date, trigram, true);
            //crawling of expired tenders
            crawlSourceForCriteria(date, trigram, false);
        }

        logger.info("Crawling for date '{}' finished", date);
    }

    /**
     * Crawls and publishes all found tenders for the given search criteria.
     *
     * @param date
     *      tender publication date
     * @param contractingAuthority
     *      contracting authority name or its substring
     * @param isActive
     *      true for active tenders, otherwise false
     */
    private void crawlSourceForCriteria(final LocalDate date, final String contractingAuthority,
        final boolean isActive) {

        logger.info("Data filtering for criteria [date: '{}', contracting authority:'{}', active:'{}']", date,
            contractingAuthority, isActive);

        HtmlPage actualPage = getSearchResultsStartPageForCriteria(date, contractingAuthority, isActive, null);

        int recordsCount = getRecordsCountWithComment(actualPage);
        //no results for criteria
        if (actualPage == null || recordsCount == 0) {
            return;
        }

        HashMap<String, Object> metaData = new HashMap<>();
        metaData.put("date", date.toString());
        metaData.put("contractingAuthority", contractingAuthority);
        metaData.put("isActive", isActive);

        if (recordsCount > RECORDS_MAX_COUNT) {
            /**
             * Appends the new filtering criterion, the supply type.
             */
            for (TenderSupplyType type : Arrays.asList(TenderSupplyType.values())) {
                logger.info("Data filtering for criteria starts [date: '{}', contracting authority:'{}', active:'{}',"
                    + " supplyType: '{}']", date, contractingAuthority, isActive, type);

                actualPage = getSearchResultsStartPageForCriteria(date, contractingAuthority, isActive, type);
                recordsCount = getRecordsCountWithComment(actualPage);
                //no results for criteria
                if (actualPage == null || recordsCount == 0) {
                    continue;
                }

                metaData.put("supplyType", type);
                processAllPages(actualPage, metaData);
            }
        } else {
            processAllPages(actualPage, metaData);
        }
    }

    /**
     * Returns count of records with commentary about the recieved count.
     *
     * @param resultPage
     *      page with results
     * @return count of results
     */
    private int getRecordsCountWithComment(final HtmlPage resultPage) {
        int recordsCount = getRecordsCount(resultPage);
        
        if (resultPage == null || recordsCount == 0) {
            logger.info("No results found");
            return 0;
        } else if (recordsCount > RECORDS_MAX_COUNT) {
            logger.warn("Count of records ({}) exceeded the limit of {}", recordsCount, RECORDS_MAX_COUNT);
        } else {
            logger.info("{} records found", recordsCount);
        }

        return recordsCount;
    }

    /**
     * Processes all pages and publishes the found details.
     *
     * @param page
     *      first page of results
     * @param metaData
     *      metadata with informations about filtering
     */
    private void processAllPages(final HtmlPage page, final HashMap<String, Object> metaData) {
        int pageNumber = 1;

        HtmlPage actualPage = page;

        while (actualPage!= null && isPageValid(actualPage)) {
            if (actualPage.getWebResponse().getStatusCode() != HTTP_SUCCESS_CODE) {
                logger.error("Received non-200 status code while trying to read from {}.", actualPage.getUrl());
                throw new UnrecoverableException("Crawling failed due to non-200 HTTP response.");
            }

            logger.info("Processing page #{}", pageNumber);
            publishTenders(actualPage, metaData);

            logger.debug("Data extracted, moving to next page.");
            pageNumber++;

            actualPage = getNextPage(actualPage, pageNumber);
        }
    }

    /**
     * Creates and publishes message for each detail found on the given results page.
     *
     * @param resutlsPage
     *      results page
     * @param metaData
     *      metadata for the published tenders
     */
    private void publishTenders(final HtmlPage resutlsPage, final HashMap<String, Object> metaData) {
        HtmlTable resultsTable = (HtmlTable) resutlsPage.getElementById(RESULTS_TABLE_ID);

        List<HtmlTableRow> tableRows = resultsTable
            .getByXPath("//table[@id='" + RESULTS_TABLE_ID + "']//tr[position() > 1]");

        for (HtmlTableRow row : tableRows) {
            HtmlAnchor detailButton = (HtmlAnchor) row.getFirstByXPath("td[last()]/a");
            if (detailButton == null) {
                continue;
            }

            try {
                createAndPublishMessage(SOURCE_DOMAIN, detailButton.click().getWebResponse().getContentAsString(),
                    metaData);
            } catch (IOException e) {
                logger.error("Unable to load detail page of tender with CIG {} because of",
                    row.getFirstChild().getTextContent(), e);
                throw new UnrecoverableException("Unable to load detail page");
            }
        }
    }

    /**
     * Method checks whether the given page is valid. That page includes table with id 'table_grid_K33_0' with more than
     * one row (first row is used for header of the results table).
     * 
     * @param page
     *      the page for validation
     * @return true for valid page, otherwise false
     */
    private Boolean isPageValid(final HtmlPage page) {
        HtmlTable table = (HtmlTable) page.getElementById(RESULTS_TABLE_ID);
        return (table != null && table.getRowCount() > 1);
    }

    /**
     * Returns next page for the given {@code page}.
     *
     * @param page
     *      origin page
     * @param pageNumber
     *      number of the page
     * @return page or null
     */
    private HtmlPage getNextPage(final HtmlPage page, final int pageNumber) {
        try {
            HtmlAnchor nextPageButton = page.getFirstByXPath("//span[@id='K33']/div/div[2]"
                + "//span[text()='" + pageNumber + "']/parent::a");

            if (nextPageButton == null) {
                return null;
            }

            nextPageButton.click();

            long time = System.currentTimeMillis();
            
            while (!isNthPage(page, pageNumber)) {
                if ((System.currentTimeMillis() - time) > RESULTSET_PAGE_TIMEOUT) {
                    logger.error("Unable to load resultset page in " + RESULTSET_PAGE_TIMEOUT + "ms");
                    throw new UnrecoverableException("Unable to load resultset page, timeout expired");
                }                
                getWebClient().waitForBackgroundJavaScript(200);
            }

            return page;
        } catch (IOException e) {
            logger.error("Unable to load next page because of", e);
            throw new UnrecoverableException("Unable to load next page");
        }
    }

    /**
     * @param context
     *      context
     * @param n
     *      page number
     * @return true if the current page is the page with the given number, otherwise false
     */
    private boolean isNthPage(final HtmlPage context, final int n) {
        return null != context.getFirstByXPath("//span[@id='K33']/div/div[2]//span[@class='currentPage'"
            + " and text()='" + n + "']");
    }

    /**
     * Returns first page of results for the given criteria.
     *
     * @param publicationDate
     *      tender publication date
     * @param contractingAuthority
     *      contracting authority name or its substring
     * @param isActive
     *      true for active tenders, otherwise false
     * @param supplyType
     *      tender supply type or null
     * @return first page
     */
    private HtmlPage getSearchResultsStartPageForCriteria(final LocalDate publicationDate,
        final String contractingAuthority, final boolean isActive, final TenderSupplyType supplyType) {
        
        try {
            HtmlPage startPage = getWebClient().getPage(String.format(DATA_URL, contractingAuthority,
                publicationDate.format(DATA_URL_DATE_FORMATTER), isActive ? 1 : 0, getSupplyType(supplyType)));

            if (!isPageValid(startPage)) {
                return null;
            }

            return startPage;
        }  catch (IOException e) {
            logger.error("Crawling failed for page with url {} with exception {}", SOURCE_DOMAIN, e);
            throw new UnrecoverableException("Unable to crawl page", e);
        }
    }

    /**
     * Returns filter form filed value for the given supply type.
     *
     * @param supplyType
     *      tender supply type, can be null
     * @return filter form field value.
     */
    private int getSupplyType(final TenderSupplyType supplyType) {
        if (supplyType == null) {
            return NONFILTER_VALUE;
        }
        
        switch (supplyType) {
            case WORKS:
                return 1;
            case SERVICES:
                return 2;
            case SUPPLIES:
                return 3;
            default:
                return NONFILTER_VALUE;
        }
    }

    /**
     * Returns count of records.
     *
     * @param resultPage
     *      page with results
     * @return count of results
     */
    private int getRecordsCount(final HtmlPage resultPage) {
        if (resultPage == null) {
            return 0;
        }
        
        DomElement countElement = resultPage.getElementById("K66");
        if (countElement == null) {
            return 0;
        }
        
        return Integer.parseInt(countElement.getTextContent().replaceAll("[^0-9]", ""), 10);
    }

    /**
     * Returns the web client.
     *
     * @return web client
     */
    private WebClient getWebClient() {
        if (webClient == null) {
            webClient = new WebClient();
            webClient.getOptions().setUseInsecureSSL(true);
            webClient.getOptions().setThrowExceptionOnScriptError(false);
            webClient.getOptions().setCssEnabled(false);

            silenceWebClient(webClient);
        }

        return webClient;
    }

    @Override
    protected void initialSetup() {
    }

    @Override
    protected void finalCleanup() {
    }

    @Override
    protected LocalDate getDefaultStartDate() {
        return DEFAULT_DATE;
    }

    @Override
    protected ChronoUnit getIncrementUnit() {
        return ChronoUnit.DAYS;
    }

    @Override
    protected String getVersion() {
        return VERSION;
    }
}
