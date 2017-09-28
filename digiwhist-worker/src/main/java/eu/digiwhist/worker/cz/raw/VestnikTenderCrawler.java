package eu.digiwhist.worker.cz.raw;

import java.net.URLEncoder;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

import com.gargoylesoftware.htmlunit.html.HtmlPage;

import eu.digiwhist.worker.raw.BaseDigiwhistIncrementalPagedSourceHttpCrawler;
import eu.dl.core.UnrecoverableException;
import eu.dl.worker.raw.utils.CrawlerUtils;

/**
 * This class is searching https://old.vestnikverejnychzakazek.cz for the delta of new items.
 *
 * @author Kuba Krafka
 */
public final class VestnikTenderCrawler extends BaseDigiwhistIncrementalPagedSourceHttpCrawler {
    private static final String VERSION = "2.0";
    private static final DateTimeFormatter SEARCH_DATE_FORMATTER = DateTimeFormatter.ofPattern("M. d. yyyy");

    private static final String BASE_URL = "https://old.vestnikverejnychzakazek.cz";
    private static final String SEARCH_FORM_URL = BASE_URL + "/en/Searching/FullTextSearch";
    private static final String PAGE_URL_TEMPLATE = SEARCH_FORM_URL +
            "?dateTimePublicationFrom=%1$s&dateTimePublicationTo=%1$s&size=%2$d&orderBy=PublishDate-asc";
    private static final String NEXT_BUTTON_XPATH = "//div[contains(@class, 't-pager')]/a[(./span[contains(@class, "
            + "'t-arrow-next')]) and (not(contains(@class, 't-state-disabled')))]";

    private static final Integer PAGE_SIZE = 20;
    private static final Integer SLEEP_LENGTH = 1000;
    private static final Integer CONNECTION_TIMEOUT = 30000;
    private static final LocalDate OLDEST_AVAILABLE_DATE = LocalDate.of(2006, 7, 6);

    /**
     * Default constructor.
     */
    public VestnikTenderCrawler() {
        super();
        getWebClient().getOptions().setTimeout(CONNECTION_TIMEOUT);
    }

    @Override
    protected HtmlPage getSearchResultsStartPageForDate(final LocalDate incrementDate) {
        try {
            // don't shut the server down
            humanize(SLEEP_LENGTH);
            String url = String.format(PAGE_URL_TEMPLATE,
                    URLEncoder.encode(incrementDate.format(SEARCH_DATE_FORMATTER), "UTF-8"), PAGE_SIZE);
            return getWebClient().getPage(url);
        } catch (final Exception e) {
            logger.error("Crawling failed for date {} with exception {}", incrementDate, e);
            throw new UnrecoverableException("Crawling failed.", e);
        }
    }

    @Override
    public HtmlPage extractDetailsFromPage(final HtmlPage page) {
        logger.debug("Processing url {}", page.getUrl());

        // TODO: maybe we should use HtmlUnit
        final Document doc = Jsoup.parse(page.getWebResponse().getContentAsString("utf-8"));
        final List<Element> links = doc.select("tr");

        for (final Element next : links) {
            final Element linkElement = next.select(".contract-link").first();
            if (linkElement != null) {
                // create the url of the tender to be downloaded
                final String tenderUrl = BASE_URL + linkElement.attr("href");

                // get the type of the tender
                final Element formNumberElement = next.select("td:nth-child(5)").first();
                final String formNumber = formNumberElement.ownText();
                final HashMap<String, Object> metaData = new HashMap<String, Object>();
                metaData.put("formNumber", formNumber);

                // add additional urls to be downloaded
                // get the additional url
                final Element additionalLinkElement = next.select("td:nth-child(8) > a").first();
                final String additionalLink = BASE_URL + additionalLinkElement.attr("href");
                final List<String> additionalUrls = new ArrayList<String>();
                additionalUrls.add(additionalLink);
                metaData.put("additionalUrls", additionalUrls);

                createAndPublishMessage(tenderUrl, metaData);
            }
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
