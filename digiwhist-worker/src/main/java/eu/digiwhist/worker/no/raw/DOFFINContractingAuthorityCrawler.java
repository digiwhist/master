package eu.digiwhist.worker.no.raw;

import java.io.IOException;
import java.util.List;

import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlPage;

import eu.digiwhist.dataaccess.dao.DAOFactory;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.worker.raw.BasePagedSourceHttpCrawler;
import eu.dl.worker.raw.utils.CrawlerUtils;

/**
 * Contract authority crawler for Norway.
 *
 * @author Michal Riha
 */
public final class DOFFINContractingAuthorityCrawler extends BasePagedSourceHttpCrawler {
    private static final String VERSION = "1";
    private static final String SOURCE_DOMAIN = "https://www.doffin.no";
    private static final String SOURCE_PROFILE_DOMAIN = "https://kgv.doffin.no";
    private static final String START_PAGE_URL = SOURCE_DOMAIN + "/en/authority";
    private static final String NEXT_BUTTON_XPATH = "//a[@class='pager-action ctm-icon-link']/i[@class='icon-forward']";

    /**
     * Default constructor.
     */
    public DOFFINContractingAuthorityCrawler() {
        super();
        // JavaScript disabled, because it was causing memory leak and is not needed here
        getWebClient().getOptions().setJavaScriptEnabled(false);
    }

    /**
     * Extracts links to detail pages for individual contracting authorities
     * from given list page. Checks for multiple authorities under the detail
     * page (in case when multiple authorities have the same organization
     * number) and processes all of them. Extracted links are published to
     * outgoing message queue for further processing.
     *
     * @param page
     *         current page with list of contracting authoritiesq
     */
    @Override
    public HtmlPage extractDetailsFromPage(final HtmlPage page) {
        final List<HtmlAnchor> organizationNumbers = ((List<HtmlAnchor>) page.getByXPath(
                "//article[@class='widget-AuthoritySearchResult widget-content " +
                        "widget-authority-search-result-widget widget']/div/div/a"));

        for (final HtmlAnchor organizationNumber : organizationNumbers) {
            // check whether the detail page is a real detail or contains
            // multiple authorities that share the same organization number
            final HtmlPage organizationNumberDetailPage;
            try {
                organizationNumberDetailPage = organizationNumber.click();
                final List<HtmlAnchor> embeddedOrganizationsLinks = ((List<HtmlAnchor>) organizationNumberDetailPage
                        .getByXPath(
                        "//table[@class='table table-hover']/tbody/tr/td/a"));

                // if there are no embedded authorities for selected organization
                // number, process the detail
                if (embeddedOrganizationsLinks.isEmpty()) {
                    createAndPublishMessage(organizationNumber.getHrefAttribute());
                } else {
                    // if there are embedded authorities, get detail page for each
                    // of them and process it separately
                    for (final HtmlAnchor tempPage : embeddedOrganizationsLinks) {
                        createAndPublishMessage(SOURCE_PROFILE_DOMAIN + tempPage.getHrefAttribute());
                    }
                }
            } catch (IOException e) {
                logger.error("Crawling failed when checking detail page {} with exception {}",
                        organizationNumber.getHrefAttribute(), e);
                throw new UnrecoverableException("Crawling failed.", e);
            }
        }

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
    protected TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }
}
