package eu.digiwhist.worker.pt.raw;

import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import com.gargoylesoftware.htmlunit.html.HtmlTable;
import com.gargoylesoftware.htmlunit.html.HtmlTableBody;
import com.gargoylesoftware.htmlunit.html.HtmlTableRow;
import eu.digiwhist.worker.raw.BaseDigiwhistIncrementalPagedSourceHttpCrawler;
import eu.dl.core.UnrecoverableException;
import eu.dl.worker.raw.utils.CrawlerUtils;
import eu.dl.worker.utils.ThreadUtils;

import java.io.IOException;
import java.time.LocalDate;
import java.time.Month;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.List;

/**
 * Tender crawler for Portugal, which crawls "Contracts".
 *
 * @author Marek Mikes
 */
public final class BASEContractTenderCrawler extends BaseDigiwhistIncrementalPagedSourceHttpCrawler {
    private static final String VERSION = "2";
    private static final String BASE_PAGE_URL = "http://www.base.gov.pt/Base/pt/ResultadosPesquisa?type=contratos";
    private static final String NEXT_BUTTON_XPATH = "//div[contains(@class, 'pagination')]//a[2]";
    private static final Integer SLEEP_LENGTH = 3000;
    private static final LocalDate FIRST_DATE_AVAILABLE = LocalDate.of(2008, Month.AUGUST, 5);

    private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd");

    /**
     * Default constructor, initializes everything important i.e. urls, xpath of elements etc.
     */
    public BASEContractTenderCrawler() {
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
    public HtmlPage getNextPage(final HtmlPage actualPage) {
        return CrawlerUtils.clickElement(actualPage, NEXT_BUTTON_XPATH);
    }

    @Override
    public HtmlPage extractDetailsFromPage(final HtmlPage page) {
        final HtmlTable table = page.getHtmlElementById("resultadosContractos");
        final List<HtmlTableBody> tableBodies = table.getBodies();
        if (tableBodies.size() == 1) {
            for (final HtmlTableRow row : tableBodies.get(0).getRows()) {
                final HtmlAnchor detailLink = row.getFirstByXPath("td/span/a");
                createAndPublishMessage(detailLink.getHrefAttribute());
            }
        } else {
            logger.error("Table does not have just one body. It has {} bodies!", tableBodies.size());
        }
        // dont shut the server down
        ThreadUtils.humanize(SLEEP_LENGTH);
        return page;
    }

    @Override
    public boolean isPageValid(final HtmlPage page) {
        return !page.getByXPath("//table[@id='resultadosContractos']/tbody/tr").isEmpty();
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
