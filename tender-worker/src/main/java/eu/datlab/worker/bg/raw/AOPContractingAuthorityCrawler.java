package eu.datlab.worker.bg.raw;

import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.worker.Message;
import eu.dl.worker.raw.BasePagedSourceHttpCrawler;
import eu.dl.worker.raw.utils.CrawlerUtils;

import java.util.List;

/**
 * Crawler to download Bulgarian data.
 *
 * @author Michal Riha
 */
public final class AOPContractingAuthorityCrawler extends BasePagedSourceHttpCrawler {
    private static final String VERSION = "1";
    private static final String START_PAGE_URL = "http://rop3-app1.aop.bg:7778/portal/page?_pageid=173," +
            "1082254&_dad=portal&_schema=PORTAL";

    private static final String NEXT_BUTTON_XPATH = "//a[@href='javascript:setPage_ff_pg_1(%d);']";

    @Override
    protected HtmlPage getSearchResultsStartPage(final Message message) {
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
    public HtmlPage extractDetailsFromPage(final HtmlPage page) {
        // extracts links from each row in table called rop_table
        final List<HtmlAnchor> detailLinks = page.getByXPath("//table[@id='rop_table']/tbody/tr/td/a");

        for (final HtmlAnchor detailLink : detailLinks) {
            createAndPublishMessage(detailLink.getHrefAttribute());
        }
        return page;
    }

    @Override
    public HtmlPage getNextPage(final HtmlPage actualPage) {
        return CrawlerUtils.clickElement(actualPage, String.format(NEXT_BUTTON_XPATH, getCurrentPageNumber()));
    }

    @Override
    public boolean isPageValid(final HtmlPage page) {
        return true;
    }

    @Override
    protected TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }
}
