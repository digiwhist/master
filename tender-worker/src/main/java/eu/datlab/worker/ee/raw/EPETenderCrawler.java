package eu.datlab.worker.ee.raw;

import com.gargoylesoftware.htmlunit.FailingHttpStatusCodeException;
import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlElement;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import com.gargoylesoftware.htmlunit.html.HtmlTableCell;
import com.gargoylesoftware.htmlunit.html.HtmlTableRow;
import com.gargoylesoftware.htmlunit.html.HtmlTextInput;
import eu.datlab.worker.raw.BaseDatlabIncrementalPagedSourceHttpCrawler;
import eu.dl.core.UnrecoverableException;
import eu.dl.worker.raw.utils.CrawlerUtils;

import java.io.IOException;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.HashMap;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Tender crawler for E-procurement in Estonia.
 *
 * @author Marek Mikes
 */
public final class EPETenderCrawler extends BaseDatlabIncrementalPagedSourceHttpCrawler {
    private static final String VERSION = "2";

    private static final String SEARCH_FORM_URL = "https://riigihanked.riik.ee/register/RegisterTeated.html";

    private static final String DATE_FROM_INPUT_XPATH = "//input[@id='txt1']";
    private static final String DATE_TO_INPUT_XPATH = "//input[@id='txt2']";
    private static final String SEARCH_BUTTON_XPATH = "//input[@id='Submit']";
    private static final String NEXT_BUTTON_XPATH = "//a[@id='linkFwd']";

    private static final LocalDate OLDEST_NOTICE_DATE = LocalDate.of(2007, 6, 8);
    private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("dd.MM.yyyy");

    private static final int MAXIMUM_NUMBER_OF_RESULTS = 500;

    private static final int EXCEPTIONS_COUNT_LIMIT = 5;

    /**
     * Initialization of the crawler.
     */
    public EPETenderCrawler() {
        super();
        getWebClient().getOptions().setJavaScriptEnabled(false);
    }

    @Override
    protected LocalDate getDefaultStartDate() {
        return OLDEST_NOTICE_DATE;
    }

    @Override
    protected HtmlPage getSearchResultsStartPageForDate(final LocalDate date) {
        try {
            int exceptionsCount = 0;
            while (true) {
                HtmlPage actualPage = getWebClient().getPage(SEARCH_FORM_URL);

                // set publication date
                String dateString = date.format(DATE_FORMATTER);
                final HtmlTextInput dateFromInput = actualPage.getFirstByXPath(DATE_FROM_INPUT_XPATH);
                dateFromInput.setText(dateString);
                final HtmlTextInput dateToInput = actualPage.getFirstByXPath(DATE_TO_INPUT_XPATH);
                dateToInput.setText(dateString);

                final HtmlElement searchButton = actualPage.getFirstByXPath(SEARCH_BUTTON_XPATH);
                try {
                    return searchButton.click();
                } catch (FailingHttpStatusCodeException e) {
                    logger.error("Server unexpectedly returned a failing http status code {} for day {}.",
                            e.getStatusCode(), dateString);
                    if (++exceptionsCount > EXCEPTIONS_COUNT_LIMIT) {
                        throw new UnrecoverableException("Unable to crawl page. Search button does not work.", e);
                    }
                }
            }
        } catch (IOException e) {
            logger.error("Crawling failed for page with url {} with exception {}", getCurrentPageUrl(), e);
            throw new UnrecoverableException("Unable to crawl page", e);
        }
    }

    @Override
    public HtmlPage extractDetailsFromPage(final HtmlPage page) {
        final List<HtmlTableRow> noticeTableRows =  page.getByXPath("//table[@id='tableview']/tbody/tr[starts-with(@id, 'informal')]");
        for (HtmlTableRow noticeTableRow : noticeTableRows) {
            try {
                final HtmlElement noticeLink = noticeTableRow.getFirstByXPath("td[12]/a/img");
                final HtmlPage noticePage = noticeLink.click();

                String noticePermalink = null;
                HtmlAnchor noticePermalinkNode = noticePage.getFirstByXPath("//a[@id='GenericLink']");

                if (noticePermalinkNode != null) {
                    noticePermalink = noticePermalinkNode.getHrefAttribute();
                } else {
                    Matcher m = Pattern.compile("https://riigihanked.riik.ee/rhr-web/#/notice/(?<id>\\d+)/print")
                        .matcher(noticePage.getUrl().toString());

                    if (m.find()) {
                        noticePermalink = "https://riigihanked.riik.ee/register/teade/" + m.group("id");
                    }
                }

                if (noticePermalink != null) {
                    final HashMap<String, Object> metaData = new HashMap<String, Object>();

                    // get name of the contracting authority
                    final HtmlTableCell contractingAuthorityCell = noticeTableRow.getFirstByXPath("td[4]");
                    final String contractingAuthority = contractingAuthorityCell.getTextContent();
                    metaData.put("contractingAuthority", contractingAuthority);

                    // get type of the tender
                    final HtmlTableCell noticeCell = noticeTableRow.getFirstByXPath("td[5]");
                    final String sourceFormType = noticeCell.getTextContent();
                    metaData.put("sourceFormType", sourceFormType);

                    HtmlTableCell dateCell = noticeTableRow.getFirstByXPath("td[7]");
                    metaData.put("publicationDate",
                        dateCell != null ? dateCell.asText() : actualDate.format(DATE_FORMATTER));
                    
                    createAndPublishMessage(noticePermalink, metaData);
                } else {
                    final HtmlTableCell noCell = noticeTableRow.getFirstByXPath("td[1]");
                    final HtmlTableCell refNoCell = noticeTableRow.getFirstByXPath("td[2]");
                    final HtmlTableCell publishedCell = noticeTableRow.getFirstByXPath("td[7]");
                    logger.warn("Unable to get notice page permalink. No {}, Ref no {}, Published {}.",
                            noCell.getTextContent().trim(), refNoCell.getTextContent(), publishedCell.getTextContent());
                }

                // number of found entries has to be less or equal than maximum of displayed entries! Maximum of
                // displayed entries is 500. If we are on the 500-th row, there are probably another entries that are
                // not displayed.
                final HtmlTableCell numberCell = noticeTableRow.getFirstByXPath("td[1]");
                if (numberCell.getTextContent().contains(Integer.toString(MAXIMUM_NUMBER_OF_RESULTS))) {
                    final HtmlTableCell publishedCell = noticeTableRow.getFirstByXPath("td[7]");
                    logger.warn(
                            "Number of results is at least {} (maximum displayed number of results) for day {}! Next "
                                    + "" + "" + "notices will be lost.",
                            MAXIMUM_NUMBER_OF_RESULTS, publishedCell.getTextContent());
                }
            } catch (final IOException e) {
                logger.error("Crawling failed for page #{} on url {} with exception {}", getCurrentPageNumber(),
                        getCurrentPageUrl(), e);
                throw new UnrecoverableException("Crawling failed.", e);
            }
        }
        return page;
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
    protected String getVersion() {
        return VERSION;
    }

    @Override
    protected ChronoUnit getIncrementUnit() {
        return ChronoUnit.DAYS;
    }
}
