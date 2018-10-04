package eu.datlab.worker.ge.raw;

import com.gargoylesoftware.htmlunit.BrowserVersion;
import com.gargoylesoftware.htmlunit.WebClient;
import com.gargoylesoftware.htmlunit.html.HtmlInput;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import com.gargoylesoftware.htmlunit.html.HtmlSelect;
import com.gargoylesoftware.htmlunit.util.Cookie;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.datlab.worker.ge.SPATenderUtils;
import eu.datlab.worker.raw.BaseDatlabIncrementalCrawler;
import eu.dl.core.UnrecoverableException;
import eu.dl.worker.utils.ThreadUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import java.io.IOException;
import java.time.LocalDate;
import java.time.Month;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * Class is searching https://tenders.procurement.gov.ge/public/?lang=en for tender details and their additional
 * parts. Also appends to each tender its list of subjects.
 *
 * @author Tomas Mrazek
 */
public final class SPATenderCrawler extends BaseDatlabIncrementalCrawler {
    private static final String VERSION = "6";
    private static final String FIRST_PAGE_URL = PublicationSources.GE_SPA + "/public?lang=en";
    private static final boolean VALIDATE_CERTIFICATES = false;
    private static final int DOWNLOAD_TIMEOUT = 30000;
    /**
     * Start date for crawling.
     */
    private static final LocalDate DEFAULT_START_DATE = LocalDate.of(2010, Month.NOVEMBER, 12);

    private static final DateTimeFormatter FORM_DATE_FORMATTER = DateTimeFormatter.ofPattern("dd.MM.yyyy");

    private static final int MAX_SLEEP_COUNT = 20;
    private static final int SLEEP_TIME = 1000;

    private final WebClient webClient;

    /**
     * Initializes web client.
     */
    public SPATenderCrawler() {
        super();
        webClient = new WebClient(BrowserVersion.CHROME);
        webClient.getOptions().setUseInsecureSSL(true);
        webClient.getOptions().setThrowExceptionOnScriptError(false);
    }

    /**
     * @return web client
     */
    private WebClient getWebClient() {
        return webClient;
    }

    @Override
    protected LocalDate getDefaultStartDate() {
        return DEFAULT_START_DATE;
    }

    @Override
    protected String getVersion() {
        return VERSION;
    }

    @Override
    protected ChronoUnit getIncrementUnit() {
        return ChronoUnit.DAYS;
    }

    @Override
    protected void initialSetup() {
        //no initial setup needed
    }

    @Override
    protected void finalCleanup() {
        //no final cleanup needed
    }

    @Override
    protected void crawlSourceForDate(final LocalDate date) {
        try {
            HtmlPage page = getWebClient().getPage(FIRST_PAGE_URL);
            setDateFilter(page, date);
            extractDetails(date);
        } catch (IOException e) {
            logger.error("Crawling for {} failed with exception {}", date, e);
            throw new UnrecoverableException("Crawling failed.", e);
        }
    }

    /**
     * Extracts all available details.
     *
     * @param statusDate
     *         status date used for filtering
     *
     * @throws IOException
     *         in case that processed page is not valid or page cannot be obtained
     */
    private void extractDetails(final LocalDate statusDate) throws IOException {
        int pageNumber = 1;
        final String statusDateString = statusDate.format(FORM_DATE_FORMATTER);

        while (true) {
            final Document resultPage = getCorrectlyLoadedResultSetPage(pageNumber, statusDateString);

            // we have to get pages count for each result page, because number of result tenders can change during
            // crawling some day. We filter by status date. So some tender disappear when it is updated.
            final int pagesCount = getPagesCount(resultPage);

            if (pageNumber > pagesCount) {
                break;
            }

            if (resultPage == null) {
                logger.error("Result page is null! Page number is {} from {}. Date {}", pageNumber, pagesCount,
                        statusDate);
                throw new UnrecoverableException("Crawling failed.");
            }
            if (!isPageValid(resultPage)) {
                logger.error("Result page is not valid! Page number is {} from {}. Date {}, url {}, source code {}.",
                        pageNumber, pagesCount, statusDate, resultPage.location(), resultPage.html());
                throw new UnrecoverableException("Crawling failed.");
            }

            logger.info("Processing page order {} (from {}) for {} with url {}", pageNumber, pagesCount, statusDate,
                    resultPage.location());
            extractDetailsFromPage(resultPage);

            pageNumber++;

            // this break is here because of optimization to prevent loading an empty page.
            // It is valid condition, because pages count can become smaller when we are on the last page.
            if (pageNumber > pagesCount) {
                break;
            }
        }
    }

    /**
     * Sets page date filter on the given date and submits filter form.
     *
     * @param page
     *         page with filter form
     * @param date
     *         date for filter
     *
     * @return page with the set filter
     * @throws IOException
     *         in case that filtering fails
     */
    private HtmlPage setDateFilter(final HtmlPage page, final LocalDate date) throws IOException {
        HtmlSelect dateType = (HtmlSelect) page.getElementById("app_date_type");
        if (dateType == null) {
            throw new UnrecoverableException("Date type select element not found.");
        }
        dateType.getOptionByText("Status date").setSelected(true);
        assert dateType.getSelectedOptions().size() == 1;

        HtmlInput dateFrom = (HtmlInput) page.getElementById("app_date_from");
        HtmlInput dateTo = (HtmlInput) page.getElementById("app_date_till");
        if (dateFrom == null || dateTo == null) {
            throw new UnrecoverableException("Date filter fields not found.");
        }
        final String dateFormatted = date.format(FORM_DATE_FORMATTER);
        dateFrom.setValueAttribute(dateFormatted);
        dateTo.setValueAttribute(dateFormatted);

        return page.getElementById("search_btn").click();
    }

    /**
     * Returns result set page for given page number.
     *
     * @param pageNumber
     *         results page number
     *
     * @return page of results
     */
    private Document getResultSetPage(final int pageNumber) {
        return getNotNullControllerActionResponse(Controller.getResultSetUrl(pageNumber));
    }

    /**
     * Downloads all details with all of their parts. Subject details are stored in metadata.
     *
     * @param page
     *         crawled page
     *
     * @throws IOException
     *         when the page cannot be obtained
     */
    protected void extractDetailsFromPage(final Document page) throws IOException {
        final Elements rows = JsoupUtils.select("#list_apps_by_subject > tbody > tr", page);

        if (isResultEmpty(rows)) {
            logger.error("No items found, actual page source code {}.", page.html());
            throw new UnrecoverableException("Crawling failed.");
        }

        for (Element row : rows) {
            String tenderId = row.id().substring(1);
            logger.debug("Tender {} processing", tenderId);

            final HashMap<Controller.Action, Document> tenderFragmentSnippets = getTenderSnippets(tenderId);
            logger.debug("Snippets downloaded");

            final HashMap<String, Document> subjectDetailSnippets = getSubjectSnippets(tenderFragmentSnippets.values());
            logger.debug("Subjects downloaded");

            final HashMap<String, String> metadataTenderSnippets = new HashMap<>();
            //main snippet is passed as sourceData
            tenderFragmentSnippets.entrySet()
                    .stream()
                    .filter(snippet -> snippet.getKey() != Controller.Action.MAIN)
                    .forEach(snippet -> {
                        metadataTenderSnippets.put(snippet.getKey().name(), snippet.getValue().html());
                    });

            final HashMap<String, String> metadataSubjectDetails = new HashMap<>();
            subjectDetailSnippets.entrySet().forEach(subject -> {
                metadataSubjectDetails.put(subject.getKey(), subject.getValue().html());
            });

            final HashMap<String, Object> metadata = new HashMap<>();
            metadata.put("snippets", metadataTenderSnippets);
            metadata.put("subjects", metadataSubjectDetails);

            final Document mainSnippet = tenderFragmentSnippets.get(Controller.Action.MAIN);

            createAndPublishMessage(getTenderPermalink(mainSnippet), mainSnippet.html(), metadata);
        }
    }

    /**
     * Downloads all detail snippets for given tender id.
     *
     * @param tenderId
     *         id of tender
     *
     * @return map of controller action and its content
     */
    private HashMap<Controller.Action, Document> getTenderSnippets(final String tenderId) {
        final HashMap<Controller.Action, Document> snippets = new HashMap<>();
        final List<Controller.Action> unwantedActions = Arrays.asList(Controller.Action.PROFILE,
                Controller.Action.RESULT_SET);

        for (Controller.Action action : Controller.Action.values()) {
            if (unwantedActions.contains(action)) {
                continue;
            }

            final Document snippet = getNotNullControllerActionResponse(Controller.getTenderSnippetUrl(tenderId,
                    action));
            snippets.put(action, snippet);
        }
        return snippets;
    }

    /**
     * Downloads all subjects snippets for subjects founded in tender snippets.
     *
     * @param tenderSnippets
     *         set of tender snippets
     *
     * @return map of subject id and its detail content
     */
    private HashMap<String, Document> getSubjectSnippets(final Collection<Document> tenderSnippets) {
        final Set<String> subjectIds = findSubjects(tenderSnippets);
        final HashMap<String, Document> subjectSnippets = new HashMap<>();

        for (String id : subjectIds) {
            final Document subjectDetail = getNotNullControllerActionResponse(Controller.getSubjectSnippetUrl(id));
            subjectSnippets.put(id, subjectDetail);
        }
        return subjectSnippets;
    }

    /**
     * Parses tender permanent link from detail page.
     *
     * @param page
     *         detail page
     *
     * @return permanent link
     */
    private String getTenderPermalink(final Document page) {
        String permalink = JsoupUtils.selectText("#app_main pre:matches((Tender Link|Procurement link)::)", page);
        return (permalink == null ? null : permalink.replaceAll("(Tender Link|Procurement link)::", "").trim());
    }

    /**
     * Returns distinct list of subjects found in given set of snippets.
     *
     * @param snippets
     *         set of detail snippets for searching subjects id
     *
     * @return ids list
     */
    private Set<String> findSubjects(final Collection<Document> snippets) {
        final Set<String> ids = new HashSet<>();

        for (Document snippet : snippets) {
            final Elements subjectDetailTriggers = JsoupUtils.select("*[onclick*=ShowProfile]", snippet);
            ids.addAll(subjectDetailTriggers.stream().map(SPATenderUtils::getSubjectId).collect(Collectors.toSet()));
        }
        return ids;
    }

    /**
     * Returns not null response content of given controller action as Jsoup Document.
     *
     * @param url
     *         action url
     *
     * @return parsed fragment
     */
    private Document getNotNullControllerActionResponse(final String url) {
        Document response = getControllerActionResponse(url);

        int sleepCount = 0;
        while (response == null && sleepCount < MAX_SLEEP_COUNT) {
            logger.info("Response is not loaded -> sleep");
            ThreadUtils.sleep(SLEEP_TIME);
            response = getControllerActionResponse(url);
            sleepCount++;
        }

        if (sleepCount < MAX_SLEEP_COUNT) {
            logger.info("Response is loaded, crawler continues");
            return response;
        } else {
            logger.error("Crawler has slept {} ms to load the response and still it is null.",
                    MAX_SLEEP_COUNT * SLEEP_TIME);
            throw new UnrecoverableException("Crawling failed.");
        }
    }

    /**
     * Returns response content of given controller action as Jsoup Document.
     *
     * @param url
     *         action url
     *
     * @return parsed fragment or null if parsing fails
     */
    private Document getControllerActionResponse(final String url) {
        try {
            Map<String, String> cookies = new HashMap<>();
            for (Cookie cookie : getWebClient().getCookieManager().getCookies()) {
                cookies.put(cookie.getName(), cookie.getValue());
            }

            return Jsoup.connect(url)
                    .header("User-Agent", getWebClient().getBrowserVersion().getUserAgent())
                    .cookies(cookies)
                    .timeout(DOWNLOAD_TIMEOUT)
                    .validateTLSCertificates(VALIDATE_CERTIFICATES)
                    .post();
        } catch (IOException e) {
            logger.warn("Controller action {} fails with exception {}.", url, e);
            return null;
        }
    }

    /**
     * Returns true if the page contains table with attribute id 'list_apps_by_subject' and div with class 'pager'.
     *
     * @param page
     *         current list page
     *
     * @return true if the page is valid, false otherwise
     */
    private boolean isPageValid(final Document page) {
        return JsoupUtils.exists("#list_apps_by_subject", page) && JsoupUtils.exists(".pager", page);
    }

    /**
     * Returns true if the result is non-empty and first row doesn't contain text "records not found".
     *
     * @param result
     *         result set
     *
     * @return true if the page contains valid search results, false otherwise
     */
    private boolean isResultEmpty(final Elements result) {
        return (result.isEmpty() || result.get(0).text().equals("records not found"));
    }

    /**
     * Returns count of result pages.
     *
     * @param resultPage
     *         valid result page
     *
     * @return count of pages
     */
    private int getPagesCount(final Document resultPage) {
        final Pattern pattern = Pattern.compile(".*page: (\\d+)/(\\d+).*");
        final Matcher matcher = pattern.matcher(JsoupUtils.selectText(".pager", resultPage).trim());

        if (matcher.find()) {
            return Integer.parseInt(matcher.group(2), 10);
        }

        logger.error("Result page does not contain page count! Url {}, source code {}.", resultPage.location(),
                resultPage.html());
        throw new UnrecoverableException("Crawling failed.");
    }

    /**
     * Gets fully loaded result page. Waits some time until the page is not fully loaded.
     *
     * @param pageNumber
     *         results page number
     * @param statusDateString
     *         status date in string
     *
     * @return fully loaded result page
     */
    private Document getCorrectlyLoadedResultSetPage(final int pageNumber, final String statusDateString) {
        Document resultPage = getResultSetPage(pageNumber);

        int sleepCount = 0;

        while (!hasResultSetPageValidRecords(resultPage, statusDateString) && sleepCount < MAX_SLEEP_COUNT) {
            logger.info("Page is not loaded -> sleep");
            ThreadUtils.sleep(SLEEP_TIME);
            resultPage = getResultSetPage(pageNumber);
            sleepCount++;
        }

        if (sleepCount < MAX_SLEEP_COUNT) {
            logger.info("Page is loaded, crawler continues");
        } else {
            logger.warn("Crawler has slept {} ms to load the page and it had not found correct record." +
                    "Page number is {}. Everything is fine when for date {} there is no record.",
                    MAX_SLEEP_COUNT * SLEEP_TIME, pageNumber, statusDateString);
        }

        return resultPage;
    }

    /**
     * Checks whether the result page contains valid result records.
     *
     * @param resultPage
     *         result page
     * @param statusDateString
     *         status date in string
     *
     * @return true when the result page contains records for the status date; otherwise false
     */
    private boolean hasResultSetPageValidRecords(final Document resultPage, final String statusDateString) {
        final Elements rows = JsoupUtils.select("#list_apps_by_subject > tbody > tr", resultPage);

        if (isResultEmpty(rows)) {
            return false;
        }

        // we have some result(s), but we do not know whether it is for the previous day (new content is not loaded yet)
        // or for the desired day

        // get tender ID from the first result record
        String tenderId = rows.get(0).id().substring(1);

        final Document chronologySnippet = getNotNullControllerActionResponse(Controller.getTenderSnippetUrl(tenderId,
                Controller.Action.CHANGELOG));

        // status day is date in the first row
        final String publicationDateTime = JsoupUtils.selectText("table > tbody > tr:nth-child(1) > td:nth-child(1)",
                chronologySnippet);

        return publicationDateTime.contains(statusDateString);
    }

}
