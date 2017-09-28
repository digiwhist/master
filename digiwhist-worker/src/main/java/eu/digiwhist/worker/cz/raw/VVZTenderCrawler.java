package eu.digiwhist.worker.cz.raw;

import java.net.URL;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import com.gargoylesoftware.htmlunit.HttpMethod;
import com.gargoylesoftware.htmlunit.WebRequest;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import com.gargoylesoftware.htmlunit.util.NameValuePair;

import eu.digiwhist.dataaccess.dto.codetables.PublicationSources;
import eu.digiwhist.worker.raw.BaseDigiwhistIncrementalPagedSourceHttpCrawler;
import eu.dl.core.UnrecoverableException;
import eu.dl.worker.raw.utils.CrawlerUtils;

/**
 * This class is searching https://vestnikverejnychzakazek.cz for the delta of new items.
 *
 * @author Tomas Posepny
 */
public final class VVZTenderCrawler extends BaseDigiwhistIncrementalPagedSourceHttpCrawler {
    private static final String VERSION = "1.0";
    private static final DateTimeFormatter SEARCH_DATE_FORMATTER = DateTimeFormatter.ofPattern("d. M. yyyy");

    private static final String BASE_URL = PublicationSources.CZ_VESTNIK;

    private static final String SEARCH_FORM_URL = BASE_URL + "/SearchForm/Search";
    private static final String PAGE_URL_TEMPLATE = SEARCH_FORM_URL +
            "?SearchFormGrid-sort=PublishDate-desc&SearchFormGrid-pageSize=%1$d";

    private static final String NEXT_BUTTON_XPATH = "//div[contains(@class, 't-pager')]/a[(./span[contains(@class, "
            + "'t-arrow-next')]) and (not(contains(@class, 't-state-disabled')))]";

    private static final Integer PAGE_SIZE = 500;
    private static final Integer SLEEP_LENGTH = 1000;
    private static final Integer CONNECTION_TIMEOUT = 30000;
    private static final LocalDate OLDEST_AVAILABLE_DATE = LocalDate.of(2016, 10, 10);

    /**
     * Default constructor.
     */
    public VVZTenderCrawler() {
        super();
        getWebClient().getOptions().setTimeout(CONNECTION_TIMEOUT);
    }

    @Override
    protected HtmlPage getSearchResultsStartPageForDate(final LocalDate incrementDate) {
        try {
            // don't shut the server down
            humanize(SLEEP_LENGTH);
            String url = String.format(PAGE_URL_TEMPLATE, PAGE_SIZE);

            WebRequest request = new WebRequest(new URL(url), HttpMethod.POST);
            List<NameValuePair> requestParameters = new ArrayList<>();
            requestParameters.add(
                    new NameValuePair("FormDatePublishedFrom", incrementDate.format(SEARCH_DATE_FORMATTER)));
            requestParameters.add(
                    new NameValuePair("FormDatePublishedTo", incrementDate.format(SEARCH_DATE_FORMATTER)));
            request.setRequestParameters(requestParameters);

            return getWebClient().getPage(request);
        } catch (final Exception e) {
            logger.error("Crawling failed for date {} with exception {}", incrementDate, e);
            throw new UnrecoverableException("Crawling failed.", e);
        }
    }

    @Override
    public HtmlPage extractDetailsFromPage(final HtmlPage page) {
        logger.debug("Processing url {}", page.getUrl());

        // TODO: maybe we should use HtmlUnit
        final Document doc = Jsoup.parse(page.getWebResponse().getContentAsString("utf-8"), BASE_URL);
        final List<Element> resultRows = doc.select("div#SearchFormGrid > table > tbody > tr:not(.k-no-data)");

        for (final Element resultRow : resultRows) {
            final Elements resultData = resultRow.select("td");

            // parse url of the tender
            String tenderUrl = resultData.get(0).select("a").first().attr("abs:href");

            // get the type of the tender
            final String formNumber = resultData.get(4).ownText();
            final HashMap<String, Object> metaData = new HashMap<String, Object>();
            metaData.put("formNumber", formNumber);

            // add additional urls to be downloaded
            // get the additional url
            final String additionalLink = resultData.get(1).select("a").first().attr("abs:href");
            final List<String> additionalUrls = new ArrayList<String>();
            additionalUrls.add(additionalLink);
            metaData.put("additionalUrls", additionalUrls);

            createAndPublishMessage(tenderUrl, metaData);
        }
        return page;
    }

    @Override
    protected LocalDate getDefaultStartDate() {
        return OLDEST_AVAILABLE_DATE;
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
    public boolean isPageValid(final HtmlPage page) {
        return true;
    }

    @Override
    protected ChronoUnit getIncrementUnit() {
        return ChronoUnit.DAYS;
    }
}
