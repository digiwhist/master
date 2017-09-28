package eu.digiwhist.worker.lv.raw;

import java.io.IOException;
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.util.List;

import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlDivision;
import com.gargoylesoftware.htmlunit.html.HtmlElement;
import com.gargoylesoftware.htmlunit.html.HtmlPage;

import eu.digiwhist.worker.raw.BaseDigiwhistIncrementalPagedSourceHttpCrawler;
import eu.dl.core.UnrecoverableException;
import eu.dl.worker.raw.utils.CrawlerUtils;

/**
 * Crawls urls of tenders from http://www.iub.gov.lv/ (Latvia).
 *
 * @author Tomas Mrazek
 */
public final class IUBTenderCrawler extends BaseDigiwhistIncrementalPagedSourceHttpCrawler {
    private static final String VERSION = "2";

    private static final String FIRST_PAGE_URL_PATTERN = "http://www.iub.gov.lv/lv/iubsearch/pb/%s/pbto/%s/adv/1/";
    private static final String DATE_URL_PART_PATTERN = "%02d%%7B%%7C%%7D%02d%%7B%%7C%%7D%4d";
    private static final String NEXT_BUTTON_XPATH = "//div[@class='pager']/div[@class='nav']/a[@class='next']";

    private static final LocalDate OLDEST_NOTICE_DATE = LocalDate.of(1999, 11, 30);

    /**
     * Default constructor.
     */
    public IUBTenderCrawler() {
        super();
        getWebClient().getOptions().setJavaScriptEnabled(false);
    }

    @Override
    protected HtmlPage getSearchResultsStartPageForDate(final LocalDate incrementDate) {
        final String dateUrlPart = String.format(DATE_URL_PART_PATTERN, incrementDate.getDayOfMonth(),
                incrementDate.getMonthValue(), incrementDate.getYear());
        final String searchResultsForDateUrl = String.format(FIRST_PAGE_URL_PATTERN, dateUrlPart, dateUrlPart);
        try {
            return getWebClient().getPage(searchResultsForDateUrl);
        } catch (IOException e) {
            logger.error("Crawling failed for page with url {} with exception {}", getCurrentPageUrl(), e);
            throw new UnrecoverableException("Unable to crawl page", e);
        }
    }

    @Override
    public HtmlPage extractDetailsFromPage(final HtmlPage page) {
        final List<HtmlElement> rows = (List<HtmlElement>) page.getByXPath(
                "//article[@class='data_table']" + "/section[@class='tbody']//section[@class='tr']");

        for (final HtmlElement row : rows) {
            //complaint is published as html fragment in outgoing message
            if (!isComplaint(row)) {
                final HtmlAnchor anchor = getDetailLinkNode(row);
                if (anchor != null) {
                    createAndPublishMessage(anchor.getHrefAttribute());
                } else {
                    logger.warn("Table row {} on page {} does not contains detail link", row.getIndex(),
                            getCurrentPageUrl());
                }
            } else {
                createAndPublishMessage(page.getUrl().toString(), row.asXml());
            }
        }
        return page;
    }

    @Override
    protected LocalDate getDefaultStartDate() {
        return OLDEST_NOTICE_DATE;
    }

    @Override
    public HtmlPage getNextPage(final HtmlPage actualPage) {
        return CrawlerUtils.clickElement(actualPage, NEXT_BUTTON_XPATH);
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
    protected ChronoUnit getIncrementUnit() {
        return ChronoUnit.DAYS;
    }

    /**
     * Returns html anchor node for detail link. Is placed in second column of the row.
     *
     * @param tableRow
     *         table row
     *
     * @return html anchor node or null if not exists
     */
    private HtmlAnchor getDetailLinkNode(final HtmlElement tableRow) {
        return tableRow.getFirstByXPath("div[@class='c2']//a[1]");
    }

    /**
     * Checks whether row contains complaint data or not.
     *
     * @param tableRow
     *         tested table row
     *
     * @return decision whether row is complaint
     */
    private boolean isComplaint(final HtmlElement tableRow) {
        HtmlDivision cell = tableRow.getFirstByXPath("div[@class='c2']");
        return cell.getTextContent().contains("Sūdzība");
    }
}
