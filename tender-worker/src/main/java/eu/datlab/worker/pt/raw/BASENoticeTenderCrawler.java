package eu.datlab.worker.pt.raw;

import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import com.gargoylesoftware.htmlunit.html.HtmlTable;
import com.gargoylesoftware.htmlunit.html.HtmlTableBody;
import com.gargoylesoftware.htmlunit.html.HtmlTableRow;

import eu.datlab.worker.raw.BaseDatlabIncrementalPagedSourceHttpCrawler;
import eu.dl.core.UnrecoverableException;
import eu.dl.worker.raw.utils.CrawlerUtils;
import eu.dl.worker.utils.ThreadUtils;
import eu.dl.worker.utils.http.URLUtils;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.time.LocalDate;
import java.time.Month;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * Tender crawler for Portugal, which crawls "Notices".
 *
 * @author Marek Mikes
 */
public final class BASENoticeTenderCrawler extends BaseDatlabIncrementalPagedSourceHttpCrawler {
    private static final String VERSION = "2";
    private static final String BASE_PAGE_URL = "http://www.base.gov.pt/Base/pt/ResultadosPesquisa?type=anuncios";
    private static final String NEXT_BUTTON_XPATH = "//div[contains(@class, 'pagination')]//a[2]";
    private static final Integer SLEEP_LENGTH = 3000;
    private static final LocalDate FIRST_DATE_AVAILABLE = LocalDate.of(2009, Month.MARCH, 16);
    private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd");

    /**
     * Default constructor, initializes everything important i.e. urls, xpath of elements etc.
     */
    public BASENoticeTenderCrawler() {
        super();
        // JavaScript disabled, because it was causing memory leak and is not needed here
        getWebClient().getOptions().setJavaScriptEnabled(false);
    }

    @Override
    protected HtmlPage getSearchResultsStartPageForDate(final LocalDate incrementDate) {
        try {
            return getWebClient().getPage(BASE_PAGE_URL + "&query=desdedatapublicacao%3D" + incrementDate.format(
                    DATE_FORMATTER) + "%26atedatapublicacao%3D" + incrementDate.format(DATE_FORMATTER));
        } catch (IOException e) {
            logger.error("Getting result for date {} failed with exception {}", incrementDate, e);
            throw new UnrecoverableException("Unable to get result page", e);
        }
    }

    @Override
    public HtmlPage extractDetailsFromPage(final HtmlPage page) {
        final HtmlTable table = page.getHtmlElementById("resultadosAnuncios");
        final List<HtmlTableBody> tableBodies = table.getBodies();
        if (tableBodies.size() == 1) {
            for (final HtmlTableRow row : tableBodies.get(0).getRows()) {
                final HtmlAnchor detailLink = row.getFirstByXPath("td/span/a");
                String detailNoticeUrl = detailLink.getHrefAttribute();

                // add additional url to be downloaded
                final HashMap<String, Object> metaData = new HashMap<>();
                try {
                    String noticeNumber = URLUtils.getUrlParameter(new URL(detailNoticeUrl), "a");
                    final String additionalLink = "http://www.base.gov" +
                            ".pt/Base/pt/ResultadosPesquisa?type=contratos&query=actoid%3D" + noticeNumber;
                    final List<String> additionalUrls = new ArrayList<>();
                    additionalUrls.add(additionalLink);
                    metaData.put("additionalUrls", additionalUrls);
                    createAndPublishMessage(detailNoticeUrl, metaData);
                } catch (MalformedURLException e) {
                    logger.error("Detail notice URL not valid {} {}", detailNoticeUrl, e);
                    throw new UnrecoverableException("Crawling failed for invalid detail notice URL.", e);
                }
            }
        } else {
            logger.error("Table does not have just one body. It has {} bodies!", tableBodies.size());
        }
        // dont shut the server down
        ThreadUtils.humanize(SLEEP_LENGTH);
        return page;
    }

    @Override
    public HtmlPage getNextPage(final HtmlPage actualPage) {
        return CrawlerUtils.clickElement(actualPage, NEXT_BUTTON_XPATH);
    }

    @Override
    public boolean isPageValid(final HtmlPage page) {
        return !page.getByXPath("//table[@id='resultadosAnuncios']/tbody/tr").isEmpty();
    }

    @Override
    protected LocalDate getDefaultStartDate() {
        return FIRST_DATE_AVAILABLE;
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
