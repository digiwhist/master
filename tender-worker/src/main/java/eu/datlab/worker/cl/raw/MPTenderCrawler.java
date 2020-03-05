package eu.datlab.worker.cl.raw;

import com.gargoylesoftware.htmlunit.StringWebResponse;
import com.gargoylesoftware.htmlunit.html.HTMLParser;
import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import eu.datlab.worker.raw.BaseDatlabIncrementalPagedSourceHttpCrawler;
import eu.dl.core.UnrecoverableException;
import org.jsoup.Connection;
import org.jsoup.Jsoup;

import java.io.IOException;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Crawler for Chile tenders.
 */
public final class MPTenderCrawler extends BaseDatlabIncrementalPagedSourceHttpCrawler {

    private static final LocalDate OLDEST_DATE = LocalDate.of(2003, 5, 10);
    private static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd");

    private Map<String, String> data = new HashMap<>();

    @Override
    protected HtmlPage getSearchResultsStartPageForDate(final LocalDate incrementDate) {
        final String date = incrementDate.format(FORMATTER) + "T23:00:00.000Z";
        data.put("textoBusqueda", "");
        data.put("idEstado", "-1");
        data.put("codigoRegion", "-1");
        data.put("idTipoLicitacion", "-1");
        data.put("fechaInicio", date);
        data.put("fechaFin", date);
        data.put("registrosPorPagina", "10");
        data.put("idOrden", "1");


        data.put("montoEstimadoTipo", "0");

        return getXhrResponse();
    }

    @Override
    protected LocalDate getDefaultStartDate() {
        return OLDEST_DATE;
    }

    @Override
    protected ChronoUnit getIncrementUnit() {
        return ChronoUnit.DAYS;
    }

    @Override
    protected String getVersion() {
        return "1";
    }

    @Override
    public HtmlPage getNextPage(final HtmlPage actualPage) {
        return getXhrResponse();
    }

    @Override
    public HtmlPage extractDetailsFromPage(final HtmlPage page) {
        // get links on details from search result page
        final List<HtmlAnchor> detailLinks = page.getByXPath("//div[@class='responsive-resultado']//a");

        for (final HtmlAnchor detailLink : detailLinks) {
            createAndPublishMessage(detailLink.getAttribute("onclick").replaceFirst("^(.*?)'", "").replaceFirst("'.*", ""));
        }

        return page;
    }

    @Override
    public boolean isPageValid(final HtmlPage page) {
        return getCurrentPageNumber() == 1 || page.getFirstByXPath("//li[@class='current']//following-sibling::a") != null;
    }

    /**
     * Response.
     *
     * @return HtmlPage
     */
    private HtmlPage getXhrResponse() {
        data.put("pagina", String.valueOf(getCurrentPageNumber()));
        try {
            Connection.Response response = Jsoup.connect("https://www.mercadopublico.cl/BuscarLicitacion/Home/Buscar")
                    .header("User-Agent", getWebClient().getBrowserVersion().getUserAgent())
                    .data(data)
                    .timeout(90000)
                    .method(Connection.Method.POST)
                    .execute();

            return HTMLParser.parseHtml(new StringWebResponse(response.body(), response.url()), getWebClient().getCurrentWindow());
        } catch (IOException e) {
            logger.error("Unable to crawl with error {}", getCurrentPageNumber(), e);
            throw new UnrecoverableException("Unable to crawl with error {}", e);
        }
    }
}
