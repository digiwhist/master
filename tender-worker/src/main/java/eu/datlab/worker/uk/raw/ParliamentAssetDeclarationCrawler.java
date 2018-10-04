package eu.datlab.worker.uk.raw;

import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.worker.Message;
import eu.dl.worker.raw.BaseHttpCrawler;

import java.io.IOException;
import java.util.List;

/**
 * Asset declarations crawler for Members' Financial Interests in UK.
 *
 * @author Marek Mikes
 */
public final class ParliamentAssetDeclarationCrawler extends BaseHttpCrawler {
    private static final String VERSION = "1";
    private static final String FIRST_PAGE_URL = "http://www.parliament" +
            ".uk/mps-lords-and-offices/standards-and-financial-interests/parliamentary-commissioner-for-standards" +
            "/registers-of-interests/register-of-members-financial-interests/";

    /**
     * Default constructor.
     */
    public ParliamentAssetDeclarationCrawler() {
        super();
        getWebClient().getOptions().setJavaScriptEnabled(false);
    }

    @Override
    protected void doWork(final Message message) {
        try {
            final HtmlPage firstPage = getWebClient().getPage(FIRST_PAGE_URL);
            final List<HtmlAnchor> yearLinks = firstPage.getByXPath(
                    "//div[@id='ctl00_ctl00_FormContent_SiteSpecificPlaceholder_PageContent_ctlMainBody" +
                            "_wrapperDiv']/div/ul/li/a[contains(@href,'contents')]");
            logger.info("Crawler found {} useful links on page {}", yearLinks.size(), firstPage.getUrl());
            for (HtmlAnchor yearLink : yearLinks) {
                final HtmlPage yearPage = yearLink.click();
                final List<HtmlAnchor> dayLinks = yearPage.getByXPath(
                        "//a[contains(@href,'/contents.htm') or contains(@href,'/part1contents.htm')]");
                logger.info("Crawler found {} pages containing info for one day on {}", dayLinks.size(),
                        yearPage.getUrl());
                for (HtmlAnchor dayLink : dayLinks) {
                    final HtmlPage listOfMembersInOneDayPage = dayLink.click();

                    // two slashes are in XPath after the div element, because crawler gets the pages with h3 in that
                    // position. User can not see the h3 in browser. Moreover, element p is mostly before element a,
                    // but not all the time.
                    final List<HtmlAnchor> memberDetailPageLinks = listOfMembersInOneDayPage
                            .getByXPath(
                            "//div[@id='mainTextBlock']//a[contains(@href,'.htm')]");
                    logger.debug("{} members is found on page {}", listOfMembersInOneDayPage.getUrl(),
                            memberDetailPageLinks.size());
                    final String listOfMembersInOneDayPageUrlString = listOfMembersInOneDayPage.getUrl().toString();
                    final String memberDetailPagePartOfLink = listOfMembersInOneDayPageUrlString.substring(0,
                            listOfMembersInOneDayPageUrlString.lastIndexOf('/') + 1);
                    for (HtmlAnchor memberDetailPageLink : memberDetailPageLinks) {
                        createAndPublishMessage(memberDetailPagePartOfLink + memberDetailPageLink.getHrefAttribute());
                    }
                }
            }
        } catch (IOException e) {
            logger.error("Crawling failed", e);
            throw new UnrecoverableException("Unable to crawl page", e);
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
