package eu.datlab.worker.ie.raw;

import com.gargoylesoftware.htmlunit.WebClient;
import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlCheckBoxInput;
import com.gargoylesoftware.htmlunit.html.HtmlElement;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import com.gargoylesoftware.htmlunit.html.HtmlSelect;
import com.gargoylesoftware.htmlunit.html.HtmlTableRow;
import com.gargoylesoftware.htmlunit.html.HtmlTextInput;
import com.gargoylesoftware.htmlunit.javascript.background.JavaScriptJobManager;
import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.datlab.worker.raw.BaseDatlabIncrementalPagedSourceHttpCrawler;
import eu.dl.core.UnrecoverableException;
import eu.dl.worker.raw.utils.CrawlerUtils;
import eu.dl.worker.utils.http.URLUtils;

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
    private static final String FIRST_PAGE_URL = DOMAIN_URL + "/ctm/supplier/publictenders";
    private static final String BASE_DETAIL_PAGE_URL = DOMAIN_URL + "/app/rfq/publicpurchase.asp?PID=";

    private static final String DATE_FROM_INPUT_XPATH = "//input[@id='SearchFilter_FromDate']";
    private static final String DATE_TO_INPUT_XPATH = "//input[@id='SearchFilter_ToDate']";
    private static final String SHOW_ALSO_EXPIRED_TENDERS_CHECKBOX_XPATH = "//input[@id='SearchFilter_ShowExpiredRft']";
    private static final String SEARCH_BUTTON_XPATH = "//a[@id='search']";
    private static final String NEXT_BUTTON_XPATH = "//li[not(@class='disabled')]/a[child::i[@class='icon-forward']]";

    private static final String FORM_TYPE_ELEMENT_ID = "SearchFilter_PublishType";

    private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("dd/MM/yyyy");

    private static final int MAX_JS_WAIT_TIMES = 10;
    private static final int SLEEP_TIME = 1000;

    /**
     * Pages have some javascript errors, they work, but generate a lot of warnings to logs, line in constructor
     * hides them.
     */
    BaseETendersTenderCrawler() {
        super();
        java.util.logging.Logger.getLogger("com.gargoylesoftware").setLevel(Level.OFF);
    }

    @Override
    protected HtmlPage getSearchResultsStartPageForDate(final LocalDate incrementDate) {
        try {
            HtmlPage actualPage = getWebClient().getPage(FIRST_PAGE_URL);

            // set form type
            HtmlSelect formType = (HtmlSelect) actualPage.getElementById(FORM_TYPE_ELEMENT_ID);
            if (formType == null) {
                throw new UnrecoverableException("Publication type select element not found.");
            }
            formType.getOptionByText(getFormTypeOptionText()).setSelected(true);
            assert formType.getSelectedOptions().size() == 1;

            // show also expired tenders
            final HtmlCheckBoxInput includeExpiredNoticesCheckBoxInput = actualPage.getFirstByXPath(
                    SHOW_ALSO_EXPIRED_TENDERS_CHECKBOX_XPATH);
            includeExpiredNoticesCheckBoxInput.setChecked(true);

            // set publication dates
            String dateString = incrementDate.format(DATE_FORMATTER);
            final HtmlTextInput dateFromInput = actualPage.getFirstByXPath(DATE_FROM_INPUT_XPATH);
            dateFromInput.setText(dateString);
            String nextDateString = incrementDate.plusDays(1).format(DATE_FORMATTER);
            final HtmlTextInput dateToInput = actualPage.getFirstByXPath(DATE_TO_INPUT_XPATH);
            dateToInput.setText(nextDateString);

            final HtmlElement searchButton = actualPage.getFirstByXPath(SEARCH_BUTTON_XPATH);

            return searchButton.click();
        } catch (IOException e) {
            logger.error("Crawling failed for page with url {} with exception {}", getCurrentPageUrl(), e);
            throw new UnrecoverableException("Unable to crawl page", e);
        }
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

        final List<HtmlTableRow> htmlTableBody = page.getByXPath("//table[@class='table table-striped table-bordered sorter']/tbody/tr");

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
            HtmlAnchor detailPageAnchor = (HtmlAnchor) htmlTableRow.getCell(2).getFirstElementChild();
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
        return true;
    }

    @Override
    protected String getVersion() {
        return VERSION;
    }

    @Override
    public HtmlPage getNextPage(final HtmlPage actualPage) {
        return CrawlerUtils.clickElement(actualPage, NEXT_BUTTON_XPATH);
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
