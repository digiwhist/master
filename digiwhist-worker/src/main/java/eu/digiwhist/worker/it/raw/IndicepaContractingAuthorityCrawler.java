package eu.digiwhist.worker.it.raw;

import java.io.IOException;
import java.util.List;

import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlPage;

import eu.digiwhist.dataaccess.dao.DAOFactory;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.worker.raw.BasePagedSourceHttpCrawler;

/**
 * Contracting authority crawler for Italian Indicepa.
 *
 * @author Michal Riha
 */
public final class IndicepaContractingAuthorityCrawler extends BasePagedSourceHttpCrawler {
    private static final String VERSION = "2";
    private static final String DOMAIN_URL = "http://www.indicepa.gov.it";
    private static final String BASE_URL = DOMAIN_URL + "/ricerca/n-risultati-alfabeto.php?lettera=";

    private char searchCharacter;

    @Override
    public HtmlPage extractDetailsFromPage(final HtmlPage page) {
        @SuppressWarnings("unchecked") List<HtmlAnchor> anchorsOnPage = (List<HtmlAnchor>) page.getByXPath(
                "//a/img[@src='../n-style/img/icon_risultati_02_scheda.png']/..");

        for (HtmlAnchor anchor : anchorsOnPage) {
            createAndPublishMessage(DOMAIN_URL + anchor.getHrefAttribute().replace("../", "/"));
        }
        return page;
    }

    @Override
    public boolean isPageValid(final HtmlPage page) {
        return true;
    }

    /**
     * Gets next page, either by clicking on next page button,
     * or returning page with companies starting with next letter in alphabet.
     * When no page is available, returns null.
     *
     * @param page
     *         page to look for next page from
     *
     * @return next page or null when no page is available
     */
    @Override
    public HtmlPage getNextPage(final HtmlPage page) {
        final HtmlAnchor anchorOnNextPage = page.getFirstByXPath(
                "//a/img[@src='../n-style/img/icon_paginazione_03_forward.png']/..");

        try {
            String pageUrl = page.getUrl().toString();
            if (anchorOnNextPage == null) {
                if (pageUrl.toLowerCase().contains("lettera=Z".toLowerCase())) {
                    return null;
                } else {
                    searchCharacter++;
                    return page.getWebClient().getPage(BASE_URL + searchCharacter);
                }
            } else if (pageUrl.contains(anchorOnNextPage.getHrefAttribute())) {
                if (pageUrl.toLowerCase().contains("lettera=Z".toLowerCase())) {
                    return null;
                } else {
                    searchCharacter++;
                    return page.getWebClient().getPage(BASE_URL + searchCharacter);
                }
            } else {
                return anchorOnNextPage.click();
            }
        } catch (IOException e) {
            logger.error("Crawling failed with exception {}", e);
            throw new UnrecoverableException("Unable to crawl page: ", e);
        }
    }

    @Override
    protected HtmlPage getSearchResultsStartPage() {
        searchCharacter = 'A';

        try {
            return getWebClient().getPage(BASE_URL + searchCharacter);
        } catch (IOException e) {
            logger.error("Crawling failed with exception {}", e);
            throw new UnrecoverableException("Unable to crawl page: ", e);
        }
    }

    @Override
    protected String getVersion() {
        return VERSION;
    }

    @Override
    protected TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }
}
