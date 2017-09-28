package eu.digiwhist.worker.bg.raw;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;

import com.gargoylesoftware.htmlunit.WebClient;
import com.gargoylesoftware.htmlunit.html.FrameWindow;
import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlElement;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import com.gargoylesoftware.htmlunit.html.HtmlTextInput;

import eu.digiwhist.dataaccess.dao.DAOFactory;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.CrawlerAuditDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.worker.raw.BasePagedSourceHttpCrawler;
import eu.dl.worker.raw.utils.CrawlerUtils;

/**
 * Tender crawler for AOP in Bulgaria.
 * The crawler can recover from crash because it saves last crawled page to DB (date filtering is not possible).
 *
 * @author Marek Mikes
 */
public final class AOPTenderCrawler extends BasePagedSourceHttpCrawler {
    private static final String VERSION = "3";

    private static final String SOURCE_DOMAIN = "http://rop3-app1.aop.bg:7778";
    private static final String FIRST_PAGE_URL = SOURCE_DOMAIN + "/portal/page?_pageid=93,"
            + "662251&_dad=portal&_schema=PORTAL";
    private static final String FORM_FILE_URL_PREFIX = "http://www.aop.bg/";

    private static final String SEARCH_BUTTON_XPATH = "//form[@id='ffind_simple']/input[@type='submit']";
    private static final String NEXT_BUTTON_XPATH = "//input[@id='direct_pg_ff_pg_0']/following-sibling::a[1]";
    private static final String PAGE_NUMBER_TEXT_INPUT_XPATH = "//input[@id='direct_pg_ff_pg_0']";

    private final WebClient webClientForDetails;

    private final CrawlerAuditDAO dao = DAOFactory
            .getDAOFactory()
            .getCrawlerAuditDAO(getName(), getVersion());

    /**
     * Default constructor, initializes everything important i.e. urls, xpath of elements etc.
     */
    public AOPTenderCrawler() {
        super();
        // JavaScript of main web client has to be enabled, otherwise paging (click on next button) does not work
        // we use another web client to see detail, otherwise it messes with going to next page in paged view
        webClientForDetails = new WebClient();
        webClientForDetails.getOptions().setUseInsecureSSL(true);
        webClientForDetails.getOptions().setThrowExceptionOnScriptError(false);
        webClientForDetails.getOptions().setJavaScriptEnabled(false);
    }

    @Override
    protected HtmlPage getSearchResultsStartPage() {
        try {
            HtmlPage startPage = getWebClient().getPage(FIRST_PAGE_URL);
            final HtmlElement searchButton = startPage.getFirstByXPath(SEARCH_BUTTON_XPATH);
            startPage = searchButton.click();

            // open page where the crawler finished if it crashed in previous run
            Integer lastCrawledPageNumber = dao.getLastCrawledPageNumberByCrawler();
            if (lastCrawledPageNumber != null) {
                String startPageNumberString = Integer.toString(lastCrawledPageNumber + 1);
                logger.warn(
                        "DB contains last crawled page number by crawler ({}) - the page is #1 for this run. It is " +
                                "correct if last run failed, otherwise you want to crawl all tenders and you should " +
                                "delete the record and run crawler again.", startPageNumberString);
                HtmlTextInput pageNumberInput = startPage.getFirstByXPath(PAGE_NUMBER_TEXT_INPUT_XPATH);
                pageNumberInput.setText(startPageNumberString);
                startPage = (HtmlPage) pageNumberInput.type('\n');
            }
            return startPage;
        } catch (IOException e) {
            logger.error("Crawling failed for {} with exception {}", FIRST_PAGE_URL, e);
            throw new UnrecoverableException("Crawling failed.", e);
        }
    }

    @Override
    public HtmlPage extractDetailsFromPage(final HtmlPage page) {
        @SuppressWarnings("unchecked") final List<HtmlAnchor> tenderDetailpageLinks = (List<HtmlAnchor>) page
                .getByXPath("//table[@id='resultaTable']/tbody/tr/"
                    + "td[preceding-sibling::td/b[text()='Преписка:' or text()='Номер на поръчката:']]/a");
        for (final HtmlAnchor tenderDetailpageLink : tenderDetailpageLinks) {
            try {
                final HtmlPage tenderDetailpage = webClientForDetails.getPage(SOURCE_DOMAIN
                    + tenderDetailpageLink.getHrefAttribute());

                final List<FrameWindow> frames = tenderDetailpage.getFrames();
                assert frames.size() == 2 : "Tender detail page should have two frame. Page " +
                        tenderDetailpage.getUrl().toString() + " has " + Integer.toString(frames.size()) + " frames.";
                final HtmlPage tenderDetailFramePage = (HtmlPage) frames.get(1).getEnclosedPage();

                @SuppressWarnings("unchecked") final List<HtmlElement> tenderFormCells = (List<HtmlElement>)
                        tenderDetailFramePage
                        .getByXPath("//a[@class='newslinks']/ancestor::td");
                for (final HtmlElement tenderFormCell : tenderFormCells) {
                    @SuppressWarnings("unchecked") final List<HtmlAnchor> tenderFormLinks = (List<HtmlAnchor>)
                            tenderFormCell
                            .getByXPath("a");

                    String formLink = tenderFormLinks.get(0).getHrefAttribute();
                    // some form links does not have prefix
                    if (!formLink.startsWith(FORM_FILE_URL_PREFIX)) {
                        formLink = FORM_FILE_URL_PREFIX + formLink;
                    }

                    if (tenderFormLinks.size() == 1) {
                        createAndPublishMessage(formLink);
                    } else if (tenderFormLinks.size() == 2) {
                        // add additional url (TED url) to be downloaded (see tender number 00692-2016-0022)
                        final HashMap<String, Object> metaData = new HashMap<>();
                        metaData.put("additionalUrlToTED", tenderFormLinks.get(1).getHrefAttribute());
                        createAndPublishMessage(formLink, metaData);
                    } else {
                        assert false : "Number of tender form links has to be 1 or 2. Page " +
                                tenderDetailFramePage.getUrl().toString() + " has " +
                                Integer.toString(tenderFormLinks.size()) + " links.";
                    }
                }
            } catch (IOException e) {
                logger.error("Crawling failed during the details extraction for {} with exception {}",
                        tenderDetailpageLink, e);
                throw new UnrecoverableException("Crawling failed.", e);
            }
        }

        // save the actual page to DB to be able recover from crash
        HtmlTextInput pageNumberInput = page.getFirstByXPath(PAGE_NUMBER_TEXT_INPUT_XPATH);
        dao.updateLastCrawledPageNumberForCrawler(Integer.parseInt(pageNumberInput.getText()));

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
    protected TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }
}
