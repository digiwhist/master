package eu.datlab.worker.uk.raw;

import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlPage;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.worker.Message;
import eu.dl.worker.raw.BaseHttpCrawler;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Crawler for UK budgets.
 */
public final class UKBudgetCrawler extends BaseHttpCrawler {
    private static final String VERSION = "1";
    private static final String BASE_URL = "https://www.gov.uk";
    private static final String LINK_PAGE = BASE_URL + "/government/collections/hmt-main-estimates";

    @Override
    protected String getVersion() {
        return VERSION;
    }

    @Override
    protected void doWork(final Message message) {
        try {
            final HtmlPage linkPage = getWebClient().getPage(LINK_PAGE);
            final List<HtmlAnchor> detailAnchors = linkPage.getByXPath("//a[@data-track-category='navDocumentCollectionLinkClicked']");

            for (HtmlAnchor detailAnchor : detailAnchors) {
                // get budget year from link text
                Matcher m = Pattern.compile("[0-9]{4}").matcher(detailAnchor.getTextContent());
                if (m.find()) {
                    final HtmlPage detailPage = detailAnchor.click();
                    final HtmlAnchor pdfAnchor = detailPage.getFirstByXPath("//div[@class='attachment-details']/h2/a");
                    HashMap<String, Object> metadata = new HashMap<>();
                    metadata.put("year", m.group());
                    
                    createAndPublishMessage(BASE_URL + pdfAnchor.getHrefAttribute(), metadata);
                } else {
                    logger.error("Budget URL title '{}' doesn't include year", detailAnchor.getTextContent());
                    throw new UnrecoverableException("Budget URL title doesn't include year");
                }
            }
        } catch (IOException e) {
            logger.error("Cannot open search page for {}", e);
            throw new UnrecoverableException("Cannot open search page", e);
        }
    }

    @Override
    protected TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }
}
