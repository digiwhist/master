package eu.datlab.worker.za.raw;

import com.gargoylesoftware.htmlunit.html.HtmlElement;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import eu.datlab.dataaccess.dto.codetables.PublicationSources;

import java.util.HashMap;
import java.util.List;

/**
 * Crawler.
 */
public final class ETendersCancelledCrawler extends BaseETendersTenderCrawler {
    private static final String RESULT_PAGE_CANCELED = PublicationSources.ZA_ETENDERS + "/content/cancelled-tenders";

    @Override
    public HtmlPage extractDetailsFromPage(final HtmlPage page) {
        final List<HtmlElement> detailRows = page.getByXPath("//tbody/tr");

        for (HtmlElement detailRow : detailRows) {
            @SuppressWarnings("unchecked") final HashMap<String, Object> metadata = new HashMap();
            metadata.put("supplyType", getTextOf(0, detailRow));
            metadata.put("tenderNumber", getTextOf(2, detailRow));
            metadata.put("publishedDate", getTextOf(3, detailRow));
            metadata.put("closedDate", getTextOf(4, detailRow));
            metadata.put("formType", "canceledTender");
            createAndPublishMessage(page.getUrl().toString(), detailRow.asXml(), metadata);
        }

        return page;
    }

    @Override
    protected String getInitialWebPage() {
        return RESULT_PAGE_CANCELED;
    }
}
