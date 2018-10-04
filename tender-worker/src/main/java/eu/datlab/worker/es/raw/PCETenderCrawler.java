package eu.datlab.worker.es.raw;

import com.gargoylesoftware.htmlunit.ElementNotFoundException;
import com.gargoylesoftware.htmlunit.FailingHttpStatusCodeException;
import com.gargoylesoftware.htmlunit.ScriptResult;
import com.gargoylesoftware.htmlunit.html.DomElement;
import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlElement;
import com.gargoylesoftware.htmlunit.html.HtmlInput;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import com.gargoylesoftware.htmlunit.html.HtmlSpan;
import com.gargoylesoftware.htmlunit.html.HtmlSubmitInput;
import com.gargoylesoftware.htmlunit.html.HtmlTable;
import com.gargoylesoftware.htmlunit.html.HtmlTableBody;
import com.gargoylesoftware.htmlunit.html.HtmlTableRow;
import eu.datlab.worker.raw.BaseDatlabIncrementalPagedSourceHttpCrawler;
import eu.dl.core.UnrecoverableException;
import eu.dl.worker.raw.utils.CrawlerUtils;

import java.io.IOException;
import java.time.LocalDate;
import java.time.Month;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.List;

/**
 * Tender crawler for Spain.
 *
 * @author Marek Mikes
 */
public final class PCETenderCrawler extends BaseDatlabIncrementalPagedSourceHttpCrawler {
    private static final String VERSION = "2";

    private static final String SOURCE_DOMAIN = "https://contrataciondelestado.es/";

    private static final String FIRST_PAGE_URL = SOURCE_DOMAIN +
            "wps/portal/!ut/p/b1/jY5JDoJAFETP4gn-pydh2QINTVBQBqU3hIUxGIaN8fy2xq1I7Sp5L1VgoHEoJUi5yxAuYKbu2d" +
            "-6Rz9P3fDuRrQszHxfxQTdggZI0qCqRGxrxC3QLAFknc-pz-qkzkWhI0QdqyCtHG51sc7HH5H4zz-DWUbIF1i6-AEWPhziebxCY7FtK" +
            "-vwKLVHMdud7FCS78s8Ig4igxIaDaMZlPL0nXVy8wIYYbKy/dl4/d5/L2dBISEvZ0FBIS9nQSEh/pw" +
            "/Z7_AVEQAI930OBRD02JPMTPG21004/act/id=0/p=javax.servlet.include" +
            ".path_info=QCPjspQCPbusquedaQCPFormularioBusqueda.jsp/321178471136/-/";

    private static final String NEXT_BUTTON_XPATH =
            "//input[@id='viewns_Z7_AVEQAI930OBRD02JPMTPG21004_:form1:footerSiguiente']";
    private static final String TENDER_NAME_XPATH =
            "//span[@id='viewns_Z7_AVEQAI930OBRD02JPMTPG21006_:form1:text_Expediente']";

    private static final LocalDate FIRST_DATE_AVAILABLE = LocalDate.of(2008, Month.JUNE, 25);

    private static final DateTimeFormatter dateFormatter = DateTimeFormatter.ofPattern("dd-MM-yyyy");

    @Override
    public HtmlPage extractDetailsFromPage(final HtmlPage page) {
        // We have to behave like we use just one tab in browser, because valid page is always the actual
        // page (the actual page holds some context)
        HtmlPage actualPage = page;

        final List<HtmlTableRow> tenderTableRows = actualPage.getByXPath("//table[@id='myTablaBusquedaCustom']/tbody/tr");
        int tenderCount = tenderTableRows.size();

        for (int i = 1; i <= tenderCount; i++) {
            try {
                final HtmlAnchor tenderDetailLink = actualPage.getFirstByXPath(
                        "//table[@id='myTablaBusquedaCustom']/tbody/tr[" + i + "]/td[1]/div/a");
                actualPage = tenderDetailLink.click();

                HtmlSpan error = actualPage.getFirstByXPath("//table[@class='TextTablaErrores']//span");
                if (error != null) {
                    if (error.getTextContent().contains("NonUniqueResultException")) {
                        logger.warn("Unable to get record {} from {} page for {} because of server is not able to return unique record",
                            i, getCurrentPageNumber(), actualDate);

                        // page doesn't include back button, use browser page navigation by javascript
                        ScriptResult result = actualPage.executeJavaScript("javascript:window.history.back();");
                        actualPage = (HtmlPage) result.getNewPage();

                        continue;
                    } else {
                        logger.error("Unexpected exception for record {} from {} page for {}: {}", i, getCurrentPageNumber(), actualDate,
                            error.getTextContent());
                        throw new UnrecoverableException("Unexpected exception");
                    }
                }

                // we want to go back to the list of tenders after visiting the tender detail page (we have to click on
                // red "Back" button)
                final HtmlAnchor backButton = actualPage.getHtmlElementById("enlace_volver");

                // get links to XML files
                try {
                    final HtmlTable table = actualPage.getHtmlElementById("myTablaDetalleVISUOE");
                    final List<HtmlTableBody> tableBodies = table.getBodies();
                    if (tableBodies.size() == 1) {
                        for (final HtmlTableRow row : tableBodies.get(0).getRows()) {
                            // skip row containing an old publication/form
                            if (row.getId().startsWith("desplegable")) {
                                continue;
                            }

                            try {
                                // Html detail leads to the page with meta http-equiv="refresh". Downloader uses for
                                // file downloading Jsoup library and unfortunately this one doesn't support this
                                // redirecting method.
                                // For reason written above, downloading xml now.
                                final HtmlAnchor xmlLink = row.getFirstByXPath("td[3]/div/a[text()='Xml']");
                                createAndPublishMessage(xmlLink.getHrefAttribute(), getWebClient()
                                        .getPage(xmlLink.getHrefAttribute()).getWebResponse().getContentAsString());
                            } catch (FailingHttpStatusCodeException e) {
                                // Probably we can not open XML publication. See tender M170004 (publication from
                                // 13/02/2017):
                                // https://contrataciondelestado.es/wps/wcm/connect/PLACE_es/Site/area/docAccCmpnt?
                                // srv=cmpnt&cmpntname=GetDocumentsById&source=library&DocumentIdParam=
                                // 098dbc99-ef45-403c-89db-2e3d5333591d
                                final DomElement tenderName = actualPage.getFirstByXPath(TENDER_NAME_XPATH);
                                assert tenderName != null;
                                logger.error("Tender {} has some XML publication which is not accessible.",
                                        tenderName.getTextContent());
                            }
                        }
                    } else {
                        logger.error("XML table does not have just one body. It has {} bodies!", tableBodies.size());
                    }
                } catch (ElementNotFoundException e) {
                    final DomElement tenderName = actualPage.getFirstByXPath(TENDER_NAME_XPATH);
                    if (tenderName != null) {
                        logger.warn("Tender {} does not have any HTML document.", tenderName.getTextContent());
                    } else {
                        logger.warn("Tender {} is inaccessible.", tenderDetailLink.getTextContent());
                        actualPage = backButton.click();
                        continue;
                    }
                }

                // get link to actual page
                final HtmlAnchor tenderDetailsPageLink = actualPage.getFirstByXPath(
                        "//a[@id='viewns_Z7_AVEQAI930OBRD02JPMTPG21006_:form1:URLgenera']");
                createAndPublishMessage(tenderDetailsPageLink.getHrefAttribute(),
                        actualPage.getWebResponse().getContentAsString());
                actualPage = backButton.click();
            } catch (final Exception ex) {
                logger.error("Crawling failed for page #{} on url {} with exception {}", getCurrentPageNumber(),
                        getCurrentPageUrl(), ex);
                throw new UnrecoverableException("Crawling failed.", ex);
            }
        }

        return actualPage;
    }

    @Override
    public boolean isPageValid(final HtmlPage page) {
        return true;
    }

    @Override
    protected LocalDate getDefaultStartDate() {
        return FIRST_DATE_AVAILABLE;
    }

    @Override
    protected HtmlPage getSearchResultsStartPageForDate(final LocalDate date) {
        try {
            final HtmlPage currentPage = getWebClient().getPage(FIRST_PAGE_URL);

            final HtmlElement form = currentPage.getHtmlElementById("viewns_Z7_AVEQAI930OBRD02JPMTPG21004_:form1");
            final HtmlInput fromDate = currentPage.getHtmlElementById(
                    "viewns_Z7_AVEQAI930OBRD02JPMTPG21004_:form1:textMinFecAnuncioMAQ2");
            final HtmlInput toDate = currentPage.getHtmlElementById(
                    "viewns_Z7_AVEQAI930OBRD02JPMTPG21004_:form1:textMaxFecAnuncioMAQ");
            final HtmlSubmitInput submit = (HtmlSubmitInput) form.getElementsByAttribute("input", "type", "submit")
                    .get(0);

            final String searchString = date.format(dateFormatter);
            fromDate.setValueAttribute(searchString);
            toDate.setValueAttribute(searchString);
            return submit.click();
        } catch (IOException e) {
            logger.error("Getting result for date {} failed with exception {}", date, e);
            throw new UnrecoverableException("Unable to get result page", e);
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
    protected ChronoUnit getIncrementUnit() {
        return ChronoUnit.DAYS;
    }
}
