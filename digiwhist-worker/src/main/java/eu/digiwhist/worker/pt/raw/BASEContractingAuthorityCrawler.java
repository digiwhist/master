package eu.digiwhist.worker.pt.raw;

import java.util.List;

import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import com.gargoylesoftware.htmlunit.html.HtmlTable;
import com.gargoylesoftware.htmlunit.html.HtmlTableBody;
import com.gargoylesoftware.htmlunit.html.HtmlTableRow;

import eu.digiwhist.dataaccess.dao.DAOFactory;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.worker.raw.BasePagedSourceHttpCrawler;
import eu.dl.worker.raw.utils.CrawlerUtils;

/**
 * Contract authority crawler for Portugal.
 *
 * @author Michal Riha
 */
public final class BASEContractingAuthorityCrawler extends BasePagedSourceHttpCrawler {
    private static final String VERSION = "1";
    private static final String SOURCE_DOMAIN = "http://www.base.gov.pt";
    private static final String START_PAGE_URL = SOURCE_DOMAIN + "/Base/pt/ResultadosPesquisa?type=entidades";
    private static final String NEXT_BUTTON_XPATH = "//a[contains(text(), 'Próxima Página')]";
    private static final int SLEEP_LENGTH = 3000;

    /**
     * Default constructor, initializes everything important i.e. urls, xpath of
     * elements etc.
     */
    public BASEContractingAuthorityCrawler() {
        super();
        // JavaScript disabled, because it was causing memory leak and is not needed here
        getWebClient().getOptions().setJavaScriptEnabled(false);
    }

    @Override
    public HtmlPage extractDetailsFromPage(final HtmlPage page) {
        final HtmlTable table = page.getHtmlElementById("resultadosEntidades");
        final List<HtmlTableBody> tableBodies = table.getBodies();
        if (tableBodies.size() == 1) {
            for (final HtmlTableRow row : tableBodies.get(0).getRows()) {
                final HtmlAnchor detailLink = row.getFirstByXPath("td/span/a");
                if (detailLink != null) {
                    createAndPublishMessage(detailLink.getHrefAttribute());
                }
            }
        } else {
            logger.error("Table does not have just one body. It has {} bodies!", tableBodies.size());
        }
        humanize(SLEEP_LENGTH);
        return page;
    }

    @Override
    protected HtmlPage getSearchResultsStartPage() {
        try {
            logger.debug("Getting the first page to start crawling from.");
            return getWebClient().getPage(START_PAGE_URL);
        } catch (final Exception e) {
            logger.error("Crawling failed for start page {} with exception {}", START_PAGE_URL, e);
            throw new UnrecoverableException("Crawling failed.", e);
        }
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
    public boolean isPageValid(final HtmlPage page) {
        return !page.getByXPath("//table[@id='resultadosEntidades']/tbody/tr").isEmpty();
    }

    @Override
    protected TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }
}
