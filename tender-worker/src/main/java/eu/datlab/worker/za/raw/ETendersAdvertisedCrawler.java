package eu.datlab.worker.za.raw;

import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlElement;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.dl.core.UnrecoverableException;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;

/**
 * Crawler.
 */
public final class ETendersAdvertisedCrawler extends BaseETendersTenderCrawler {
    private static final String RESULT_PAGE_ADVERTISED = PublicationSources.ZA_ETENDERS + "/content/advertised-tenders";

    @Override
    public HtmlPage extractDetailsFromPage(final HtmlPage page) {
        final List<HtmlElement> detailRows = page.getByXPath("//tbody/tr");

        for (HtmlElement detailRow : detailRows) {
            @SuppressWarnings("unchecked") final HashMap<String, Object> metadata = new HashMap();
            metadata.put("supplyType", getTextOf(0, detailRow));
            metadata.put("tenderNumber", getTextOf(2, detailRow));
            metadata.put("publishedDate", getTextOf(3, detailRow));
            metadata.put("closedDate", getTextOf(4, detailRow));
            metadata.put("formType", "advertisedTender");

            try {
                final HtmlPage detailPage = ((HtmlAnchor) detailRow.getFirstByXPath("td[2]/a")).click();
                metadata.put("detailPage", detailPage.getWebResponse().getContentAsString());
            } catch (IOException e) {
                logger.error("Unable to open page with error: {}", e);
                throw new UnrecoverableException("Unable to open page with error: {}", e);
            }

            createAndPublishMessage(PublicationSources.ZA_ETENDERS + ((HtmlAnchor) detailRow.getFirstByXPath("td[2]/a"))
                    .getHrefAttribute(), detailRow.asXml(), metadata);
        }

        return page;
    }

    @Override
    protected String getInitialWebPage() {
        return RESULT_PAGE_ADVERTISED;
    }
}
