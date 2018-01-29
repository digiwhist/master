package eu.digiwhist.worker.lt.raw;

import java.io.IOException;
import java.time.LocalDate;
import java.time.Month;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.HashMap;
import java.util.List;

import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import com.gargoylesoftware.htmlunit.html.HtmlSpan;

import eu.digiwhist.worker.raw.BaseDigiwhistIncrementalPagedSourceHttpCrawler;
import eu.dl.core.UnrecoverableException;
import eu.dl.worker.raw.utils.CrawlerUtils;
import eu.dl.worker.utils.http.URLUtils;

/**
 * Tender crawler for CVPIS in Lithuania.
 *
 * @author Marek Mikes
 */
public final class CVPISTenderCrawler extends BaseDigiwhistIncrementalPagedSourceHttpCrawler {
    private static final String VERSION = "1";

    private static final LocalDate OLDEST_TENDER_DATE = LocalDate.of(2008, Month.SEPTEMBER, 19);
    private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd");

    private static final int PAGE_SIZE = 50; // valid numbers are  10, 20, 30, 40 or 50
    private static final String RESULT_PAGE_URL_PATTERN = "http://cvpp.lt/index"
            + ".php?option=com_vpt&Itemid=63&task=search&theme=new&vpt_noticetype=0" +
            "&vpt_proceduretype=0&vpt_reporttype=0&filter_from=%s&filter_to=%s&filter_limit=%s&limitstart=%s";

    private static final String OLD_TENDER_ID_URL_PARAMETER = "legacy_id";
    private static final String NEW_TENDER_ID_URL_PARAMETER = "tender_id";

    // test if text value ('text()') is a number is done by 'number(text())=number(text())'. XPath function number()
    // returns NaN if parameter is not number and comparison NaN and NaN returns false.
    private static final String NEXT_BUTTON_XPATH =
            "//div[@class='pagination-txt']/span[number(text())=number(text())]/following-sibling::span/a";

    /**
     * Initialization of the crawler.
     */
    public CVPISTenderCrawler() {
        super();
        // do not throw exception on failing status code. The FailingHttpStatusCodeException (400) is thrown when Google
        // map on detail page does not know entered address. E.g.:
        // http://cvpp.lt/index.php?option=com_vpt&theme=new&task=view&tender_id=287744
        getWebClient().getOptions().setThrowExceptionOnFailingStatusCode(false);
        getWebClient().getOptions().setJavaScriptEnabled(false);
    }

    @Override
    protected HtmlPage getSearchResultsStartPageForDate(final LocalDate incrementDate) {
        String url = String.format(RESULT_PAGE_URL_PATTERN, incrementDate.format(DATE_FORMATTER),
                incrementDate.plusDays(1).format(DATE_FORMATTER), Integer.toString(PAGE_SIZE), Integer.toString(0));
        try {
            return getWebClient().getPage(url);
        } catch (final IOException e) {
            logger.error("Crawling failed for date {} page url {} with exception {}", incrementDate, url, e);
            throw new UnrecoverableException("Crawling failed.", e);
        }
    }

    @Override
    public HtmlPage getNextPage(final HtmlPage actualPage) {
        return CrawlerUtils.clickElement(actualPage, NEXT_BUTTON_XPATH);
    }

    @Override
    public HtmlPage extractDetailsFromPage(final HtmlPage page) {
        final List<HtmlAnchor> tenderDetailPageLinks = (List<HtmlAnchor>) page.getByXPath(
                "//div/table/tbody/tr/td/div/h3/a");
        for (HtmlAnchor tenderDetailPageLink : tenderDetailPageLinks) {
            try {
                HtmlPage tenderDetailPage = tenderDetailPageLink.click();

                final HashMap<String, Object> metaData = new HashMap<>();
                String tenderId = null;
                String tenderDetailPageUrlString = tenderDetailPage.getUrl().toString();
                if (tenderDetailPageUrlString.contains(OLD_TENDER_ID_URL_PARAMETER)) {
                    tenderId = URLUtils.getUrlParameter(tenderDetailPage.getUrl(), OLD_TENDER_ID_URL_PARAMETER);
                } else if (tenderDetailPageUrlString.contains(NEW_TENDER_ID_URL_PARAMETER)) {
                    tenderId = URLUtils.getUrlParameter(tenderDetailPage.getUrl(), NEW_TENDER_ID_URL_PARAMETER);
                } else {
                    assert false;
                }
                metaData.put("tenderId", tenderId);

                HtmlSpan dateSpan = ((HtmlSpan) tenderDetailPageLink.getEnclosingElement("div")
                    .getFirstByXPath("./div[contains(.,'Paskelbtas:')]/span"));                
                metaData.put("publicationDate",
                    dateSpan != null ? dateSpan.asText() : actualDate.format(DATE_FORMATTER));

                // send form links to queue
                final List<HtmlAnchor> formLinks = (List<HtmlAnchor>) tenderDetailPage.getByXPath(
                    "//div/div[@class='doc-links']/a[contains(@href,'task=viewnotice')]");
                for (HtmlAnchor formLink : formLinks) {
                    createAndPublishMessage(formLink.getHrefAttribute(), metaData);
                }

                // send lot links to queue
                HtmlAnchor lotListPageLink = tenderDetailPage.getFirstByXPath(
                        "//div/div[@class='doc-links']/a[contains(@href,'task=ataskaitos')]");
                if (lotListPageLink != null) {
                    HtmlPage lotListPage = lotListPageLink.click();
                    final List<HtmlAnchor> lotLinks = (List<HtmlAnchor>) lotListPage.getByXPath(
                            "//tr[@id='vptpublic_main_1']/td[5]/a");
                    for (HtmlAnchor lotLink : lotLinks) {
                        createAndPublishMessage(lotLink.getHrefAttribute(), metaData);
                    }
                }
            } catch (IOException e) {
                logger.error("Unable to crawl url {}", getCurrentPageUrl(), e);
                throw new UnrecoverableException("Crawling failed.", e);
            }
        }
        return page;
    }

    @Override
    public boolean isPageValid(final HtmlPage page) {
        return true;
    }

    @Override
    protected LocalDate getDefaultStartDate() {
        return OLDEST_TENDER_DATE;
    }

    @Override
    protected String getVersion() {
        return VERSION;
    }

    @Override
    protected ChronoUnit getIncrementUnit() {
        return ChronoUnit.DAYS;
    }
}
