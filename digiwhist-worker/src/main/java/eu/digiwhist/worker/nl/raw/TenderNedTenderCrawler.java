package eu.digiwhist.worker.nl.raw;

import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlElement;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import com.gargoylesoftware.htmlunit.html.HtmlRadioButtonInput;
import com.gargoylesoftware.htmlunit.html.HtmlSubmitInput;
import com.gargoylesoftware.htmlunit.html.HtmlTextInput;
import eu.digiwhist.dataaccess.dto.codetables.PublicationSources;
import eu.digiwhist.worker.raw.BaseDigiwhistIncrementalPagedSourceHttpCrawler;
import eu.dl.core.UnrecoverableException;
import eu.dl.worker.raw.utils.CrawlerUtils;

import java.io.IOException;
import java.time.LocalDate;
import java.time.Month;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * Tender crawler for TenderNed in Netherlands.
 *
 * @author Marek Mikes
 */
public final class TenderNedTenderCrawler extends BaseDigiwhistIncrementalPagedSourceHttpCrawler {
    private static final String VERSION = "2";
    private static final String SOURCE_DOMAIN = PublicationSources.NL_NED;

    private static final String FIRST_PAGE_URL = SOURCE_DOMAIN +
            "/tenderned-web/aankondiging/overzicht/aankondigingenplatform";

    private static final String DATE_SPAN_RADIO_ID = "aankondigingenplatform:publicatie-datum:5";
    private static final String DATE_FROM_INPUT_ID = "aankondigingenplatform:publicatie-date-van";
    private static final String DATE_TO_INPUT_ID = "aankondigingenplatform:publicatie-date-tot";
    private static final String SEARCH_BUTTON_XPATH =
            "//ol[li/strong[contains(text(),'Publicatiedatum')]]//input[@type='submit']";
    private static final String NEXT_BUTTON_XPATH = "//li[@class='next']/a";

    private static final LocalDate OLDEST_NOTICE_DATE = LocalDate.of(2010, Month.DECEMBER, 3);

    private static final DateTimeFormatter DATE_PARAMETER_FORMATTER = DateTimeFormatter.ofPattern("dd-MM-yyyy");

    private static final String PUBLICATION_RELATIVE_URL_PREFIX = "/tenderned-web/aankondiging/detail/publicatie/akid/";

    /**
     * Default constructor.
     */
    public TenderNedTenderCrawler() {
        super();
        // JavaScript disabled, because it was causing memory leak (first 100 messages: more than 400 MB RAM usage vs
        // less than 200 MB RAM) and is not needed here and the crawler is faster now
        getWebClient().getOptions().setJavaScriptEnabled(false);
    }

    @Override
    protected HtmlPage getSearchResultsStartPageForDate(final LocalDate incrementDate) {
        try {
            final HtmlPage page = getWebClient().getPage(FIRST_PAGE_URL);
            final HtmlRadioButtonInput dateType = (HtmlRadioButtonInput) page.getElementById(DATE_SPAN_RADIO_ID);
            final HtmlTextInput dateFrom = (HtmlTextInput) page.getElementById(DATE_FROM_INPUT_ID);
            final HtmlTextInput dateTo = (HtmlTextInput) page.getElementById(DATE_TO_INPUT_ID);
            final HtmlSubmitInput submit = page.getFirstByXPath(SEARCH_BUTTON_XPATH);
            dateType.setChecked(true);
            dateFrom.setValueAttribute(incrementDate.format(DATE_PARAMETER_FORMATTER));
            dateTo.setValueAttribute(incrementDate.format(DATE_PARAMETER_FORMATTER));
            return submit.click();
        } catch (Exception e) {
            logger.error("Searching results for date {} fails.", getCurrentPageUrl());
            throw new UnrecoverableException("Searching results for date fails", e);
        }
    }

    @Override
    public HtmlPage extractDetailsFromPage(final HtmlPage page) {
        final List<HtmlAnchor> announcementLinks = ((List<HtmlAnchor>) page.getByXPath(
                "//span[@id='T909:publicatie-results']/ol/li/h3/a"));

        for (HtmlAnchor announcementLink : announcementLinks) {
            try {
                HtmlPage announcementPage = announcementLink.click();

                final HtmlAnchor publicationLink = announcementPage.getFirstByXPath("//a[@title='Publicatie']");
                if (publicationLink == null) {
                    // the announcement is withdrawn for technical reasons and the crawler is still on results page,
                    // where warning message is displayed
                    assert !announcementPage.getByXPath("//div[@id='T909:notification-errors']").isEmpty();
                    final HtmlTextInput dateFrom = (HtmlTextInput) announcementPage.getElementById(DATE_FROM_INPUT_ID);
                    logger.warn("Announcement \"{}\" is withdrawn for technical reasons (publication day {}).",
                            announcementLink.getTextContent(), dateFrom.getValueAttribute());
                    continue;
                }

                // One tender has different urls for two searches. See:
                //   https://www.tenderned.nl/tenderned-web/aankondiging/detail/publicatie/akid/
                //     cdf73d2a5c639bd73bf9945a70aaaf5f/pageId/D909A/huidigemenu/aankondigingen/cid/888019/cvp/join
                //   https://www.tenderned.nl/tenderned-web/aankondiging/detail/publicatie/akid/
                //     cdf73d2a5c639bd73bf9945a70aaaf5f/pageId/D909A/huidigemenu/aankondigingen/cid/1149348/cvp/join
                // We want to get the same URL because of persistent ID. So we get prefix:
                //   https://www.tenderned.nl/tenderned-web/aankondiging/detail/publicatie/akid/
                //     cdf73d2a5c639bd73bf9945a70aaaf5f
                String publicationUrl = publicationLink.getHrefAttribute();
                if (!publicationUrl.startsWith(PUBLICATION_RELATIVE_URL_PREFIX)) {
                    logger.error("Relative path of publication has unknown structure. The path is {} on url {} ",
                            publicationUrl, getCurrentPageUrl());
                    throw new UnrecoverableException("Crawling failed.");
                }
                publicationUrl = publicationUrl.substring(0, publicationUrl.indexOf("/",
                        PUBLICATION_RELATIVE_URL_PREFIX.length()));
                publicationUrl = SOURCE_DOMAIN + publicationUrl;

                // add additional urls to be downloaded
                final HashMap<String, Object> metaData = new HashMap<>();
                final List<String> additionalUrls = new ArrayList<>();
                final String overviewUrl = announcementPage.getUrl().toString();
                additionalUrls.add(overviewUrl);
                final HtmlAnchor documentsLink = announcementPage.getFirstByXPath("//a[@title='Documenten']");
                if (documentsLink != null) {
                    final String documentsUrl = SOURCE_DOMAIN + documentsLink.getHrefAttribute();
                    additionalUrls.add(documentsUrl);
                } else {
                    logger.info("An announcement {} does not have documents.", overviewUrl);
                }
                metaData.put("additionalUrls", additionalUrls);

                createAndPublishMessage(publicationUrl, metaData);
            } catch (IOException e) {
                logger.error("Crawling failed for page #{} on url {} with exception {}", getCurrentPageNumber(),
                        getCurrentPageUrl(), e);
                throw new UnrecoverableException("Crawling failed.", e);
            }
        }

        if (announcementLinks.isEmpty()) {
            HtmlElement noResultsTextElement = page.getFirstByXPath("//span[@id='T909:publicatie-results']/div/div/p");
            if (noResultsTextElement == null || !noResultsTextElement.getTextContent().trim().equals(
                    "Er zijn geen aankondigingen gevonden op basis van de door u ingevulde zoekcriteria")) {
                logger.error("Page has no results and relevant text is missing. Source code is {}.",
                        page.getWebResponse().getContentAsString());
                throw new UnrecoverableException("Crawling failed.");
            } else {
                final HtmlTextInput dateFrom = (HtmlTextInput) page.getElementById(DATE_FROM_INPUT_ID);
                logger.info("There are no results for publication date {}.", dateFrom.getValueAttribute());
            }
        }

        return page;
    }

    @Override
    public HtmlPage getNextPage(final HtmlPage actualPage) {
        return CrawlerUtils.clickElement(actualPage, NEXT_BUTTON_XPATH);
    }

    @Override
    protected String getVersion() {
        return VERSION;
    }

    @Override
    public boolean isPageValid(final HtmlPage page) {
        return true;
    }

    @Override
    protected LocalDate getDefaultStartDate() {
        return OLDEST_NOTICE_DATE;
    }

    @Override
    protected ChronoUnit getIncrementUnit() {
        return ChronoUnit.DAYS;
    }
}
