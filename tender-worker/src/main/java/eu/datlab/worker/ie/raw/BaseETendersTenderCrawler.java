package eu.datlab.worker.ie.raw;

import com.gargoylesoftware.htmlunit.StringWebResponse;
import com.gargoylesoftware.htmlunit.WebClient;
import com.gargoylesoftware.htmlunit.html.HTMLParser;
import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import com.gargoylesoftware.htmlunit.html.HtmlTableRow;
import com.gargoylesoftware.htmlunit.javascript.background.JavaScriptJobManager;
import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.datlab.worker.raw.BaseDatlabIncrementalPagedSourceHttpCrawler;
import eu.dl.core.UnrecoverableException;
import eu.dl.worker.utils.http.URLUtils;
import org.jsoup.Connection;
import org.jsoup.Jsoup;

import java.io.IOException;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.logging.Level;

import static eu.datlab.worker.ie.ETendersTenderConstants.SYSTEM_ID_COLUMN_METADATA_KEY;
import static eu.datlab.worker.ie.ETendersTenderConstants.TENDER_REFERENCE_COLUMN_METADATA_KEY;

/**
 * Base tender crawler for Ireland ETenders - it crawls just one form type (CFT or CA).
 *
 * @author Michal Riha
 */
abstract class BaseETendersTenderCrawler extends BaseDatlabIncrementalPagedSourceHttpCrawler {
    private static final String VERSION = "1";

    private static final String DOMAIN_URL = PublicationSources.IE_ETENDERS;
    private static final String FIRST_PAGE_URL = DOMAIN_URL + "/ctm/Supplier/PublicTenders";
    private static final String BASE_DETAIL_PAGE_URL = DOMAIN_URL + "/app/rfq/publicpurchase.asp?PID=";

    private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("dd/MM/yyyy");

    private static final int MAX_JS_WAIT_TIMES = 10;
    private static final int SLEEP_TIME = 1000;

    private static final String PAGE_SIZE = "25";

    /**
     * Pages have some javascript errors, they work, but generate a lot of warnings to logs, line in constructor
     * hides them.
     */
    BaseETendersTenderCrawler() {
        super();
        java.util.logging.Logger.getLogger("com.gargoylesoftware").setLevel(Level.OFF);
    }

    /**
     * Sends POST request with search details and parses HTML page.
     * @param date date for search
     * @param pageNumber page number for search
     * @return result page
     */
    private HtmlPage getPageViaPOSTMethod(final LocalDate date, final String pageNumber) {
        String fromDate = date.format(DATE_FORMATTER);
        String toDate = date.plusDays(1).format(DATE_FORMATTER);

        Connection.Response response = null;
        try {
            response = Jsoup.connect(FIRST_PAGE_URL)
                    .header("User-Agent", getWebClient().getBrowserVersion().getUserAgent())
                    .data("SearchFilter.PublishType", getFormTypeOptionText())
                    .data("SearchFilter.FromDate", fromDate)
                    .data("SearchFilter.ToDate", toDate)
                    .data("SearchFilter.SortField", "None")
                    .data("SearchFilter.SortDirection", "None")
                    .data("SearchFilter.Reference", "")
                    .data("SearchFilter.TenderId", "0")
                    .data("SearchFilter.OperatorId", "1")
                    .data("Branding", "ETENDERS_SIMPLE")
                    .data("SavedCategoryId", "")
                    .data("SavedUnitAndName", "")
                    .data("TextFilter", "")
                    .data("SavedCategoryId", "")
                    .data("CpvContainer.CpvCodes", "")
                    .data("CpvContainer.CpvIds", "")
                    .data("CpvContainer.CpvMain", "")
                    .data("CpvContainer.ContractType", "")
                    .data("CpvContainer.CpvIds", "")
                    .data("CpvContainer.IsMandatory", "True")
                    .data("SearchFilter.ShowExpiredRft", "true")
                    .data("SearchFilter.ShowExpiredRft", "false")
                    .data("SearchFilter.PagingInfo.PageNumber", pageNumber)
                    .data("SearchFilter.PagingInfo.PageSize", PAGE_SIZE)
                    .timeout(90000)
                    .method(Connection.Method.POST)
                    .execute();
        }  catch (IOException e) {
        logger.error("Sending POST request failed with exception {}", e.getMessage());
        throw new UnrecoverableException("Could not send POST request", e);
    }
        try {
            return HTMLParser.parseHtml(new StringWebResponse(response.body(), response.url()),
                    getWebClient().getCurrentWindow());
        } catch (IOException e) {
            logger.error("Getting response failed with exception {}", e.getMessage());
            throw new UnrecoverableException("Could not get response", e);
        }
    }

    @Override
    protected HtmlPage getSearchResultsStartPageForDate(final LocalDate incrementDate) {
            return getPageViaPOSTMethod(incrementDate, "1");
    }

    @Override
    public HtmlPage extractDetailsFromPage(final HtmlPage page) {
        //wait for all javaScripts to finish on detail list
        JavaScriptJobManager manager = page.getEnclosingWindow().getJobManager();
        int waitTriedTimes = 0;
        while (manager.getJobCount() > 0 && waitTriedTimes < MAX_JS_WAIT_TIMES) {
            try {
                Thread.sleep(SLEEP_TIME);
            } catch (InterruptedException e) {
                logger.error("Waiting for next page failed: {}", e);
            }
            waitTriedTimes++;
        }

        // we use another web client to see detail, otherwise it messes with going to next page in paged view
        WebClient webClientForDetails = new WebClient();
        webClientForDetails.getOptions().setUseInsecureSSL(true);
        webClientForDetails.getOptions().setThrowExceptionOnScriptError(false);

        final List<HtmlTableRow> htmlTableBody = page.getByXPath("//div[@id='searchResultContainer']/table/tbody/tr");

        for (HtmlTableRow htmlTableRow : htmlTableBody) {
            // we do not want to get tender created today. It can happen during crawling of tenders from today
            LocalDate tenderDate = LocalDate.parse(htmlTableRow.getCell(3).getTextContent().trim(), DATE_FORMATTER);
            if (tenderDate.isEqual(LocalDate.now())) {
                continue;
            }

            // add additional information as metadata
            final HashMap<String, Object> metaData = new HashMap<>();
            metaData.put(SYSTEM_ID_COLUMN_METADATA_KEY, htmlTableRow.getCell(0).getTextContent().trim());
            metaData.put(TENDER_REFERENCE_COLUMN_METADATA_KEY, htmlTableRow.getCell(1).getTextContent().trim());

            // get URL to detail page. We have to get PID, which is part of URL. It is not the same as "System Id" from
            // the first column.
            // See https://irl.eu-supply.com/app/rfq/publicpurchase.asp?PID=46 ("System Id" is 10040)
            HtmlAnchor detailPageAnchor = htmlTableRow.getCell(2).getFirstByXPath("a");
            final String pid = URLUtils.getUrlParameter(detailPageAnchor.getHrefAttribute(), "PID");
            final String detailPageLink = BASE_DETAIL_PAGE_URL + pid;

            try {
                final HtmlPage detailPage = webClientForDetails.getPage(detailPageLink);

                HtmlAnchor profileUrl = detailPage.getFirstByXPath("//a[text()[contains(.,'View profile')]]");
                if (profileUrl != null) {
                    HtmlPage profilePage = profileUrl.click();
                    metaData.put("additionalUrls",
                            Collections.singletonList(profilePage.getUrl()));
                }

                createAndPublishMessage(detailPage.getUrl().toString(),
                        detailPage.getWebResponse().getContentAsString(), metaData);

                // the XPath can not be just "//a[@target='_blank']"
                // see https://irl.eu-supply.com/app/rfq/publicpurchase.asp?PID=70844
                List<HtmlAnchor> documentsOnDetailPage = detailPage.getByXPath(
                        "//span[text()='Date of publishing']/../../../tr/td/a[@target='_blank']");
                for (HtmlAnchor documentOnDetailPage : documentsOnDetailPage) {
                    final String documentDetailPageUrl = DOMAIN_URL + documentOnDetailPage.getHrefAttribute().trim();
                    createAndPublishMessage(documentDetailPageUrl, metaData);
                }
            } catch (IOException e) {
                logger.error("Crawling failed for page #{} on url {} with exception {}", getCurrentPageNumber(),
                        getCurrentPageUrl(), e);
                webClientForDetails.close();
                throw new UnrecoverableException("Crawling failed.", e);
            }
        }

        webClientForDetails.close();
        
        return page;
    }

    @Override
    public boolean isPageValid(final HtmlPage page) {
        return !page.getByXPath("//div[@id='searchResultContainer']/table/tbody/tr").isEmpty();
    }

    @Override
    protected String getVersion() {
        return VERSION;
    }

    @Override
    public HtmlPage getNextPage(final HtmlPage actualPage) {
        return getPageViaPOSTMethod(actualDate, String.valueOf(getCurrentPageNumber() + 1));
    }

    @Override
    protected ChronoUnit getIncrementUnit() {
        return ChronoUnit.DAYS;
    }

    /**
     * @return text in form type option in filter dialog.
     */
    protected abstract String getFormTypeOptionText();
}
