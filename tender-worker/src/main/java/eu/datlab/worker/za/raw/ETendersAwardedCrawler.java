package eu.datlab.worker.za.raw;

import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlElement;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.dl.core.UnrecoverableException;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * Crawler.
 */
public final class ETendersAwardedCrawler extends BaseETendersTenderCrawler {
    private static final String RESULT_PAGE_AWARDED = PublicationSources.ZA_ETENDERS + "/content/awarded-tenders";

    @Override
    public HtmlPage extractDetailsFromPage(final HtmlPage page) {
        final List<HtmlElement> detailRows = page.getByXPath("//tbody/tr");

        for (HtmlElement detailRow : detailRows) {
            @SuppressWarnings("unchecked") final HashMap<String, Object> metadata = new HashMap();
            metadata.put("supplyType", ((HtmlElement) detailRow.getFirstByXPath("td[1]")).getTextContent());
            metadata.put("tenderNumber", ((HtmlElement) detailRow.getFirstByXPath("td[3]")).getTextContent());
            metadata.put("formType", "awardedTender");
            final HtmlAnchor detailUrl = detailRow.getFirstByXPath("td[2]/a");
            try {
                final HtmlPage detailPage = detailUrl.click();
                metadata.put("detailPage", detailPage.getWebResponse().getContentAsString());

                final List<String> bidders = new ArrayList<>();
                final List<HtmlAnchor> bidderAnchors = detailPage.getByXPath("//table[1]/tbody/tr/td[1]/a");
                for (HtmlAnchor bidderAnchor : bidderAnchors) {
                    final HtmlPage bidder = getWebClient().getPage(PublicationSources.ZA_ETENDERS + bidderAnchor.getHrefAttribute());
                    bidders.add(bidder.getWebResponse().getContentAsString());
                }
                metadata.put("bidders", bidders);
            } catch (IOException e) {
                logger.error("Unable to open page with error: {}", e);
                throw new UnrecoverableException("Unable to open page with error: {}", e);
            }
            createAndPublishMessage(PublicationSources.ZA_ETENDERS + detailUrl.getHrefAttribute(), detailRow.asXml(), metadata);
        }

        return page;
    }

    @Override
    protected String getInitialWebPage() {
        return RESULT_PAGE_AWARDED;
    }
}
