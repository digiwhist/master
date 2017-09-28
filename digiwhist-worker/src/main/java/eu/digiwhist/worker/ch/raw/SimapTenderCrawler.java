package eu.digiwhist.worker.ch.raw;

import java.io.IOException;
import java.net.ConnectException;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.HashMap;
import java.util.List;

import com.gargoylesoftware.htmlunit.FailingHttpStatusCodeException;
import com.gargoylesoftware.htmlunit.html.HtmlElement;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import com.gargoylesoftware.htmlunit.html.HtmlRadioButtonInput;
import com.gargoylesoftware.htmlunit.html.HtmlTableCell;
import com.gargoylesoftware.htmlunit.html.HtmlTableRow;
import com.gargoylesoftware.htmlunit.html.HtmlTextInput;

import eu.digiwhist.worker.raw.BaseDigiwhistIncrementalPagedSourceHttpCrawler;
import eu.dl.core.UnrecoverableException;
import eu.dl.worker.raw.utils.CrawlerUtils;

/**
 * Tender crawler for simap in Switzerland.
 *
 * @author Marek Mikes
 */
public final class SimapTenderCrawler extends BaseDigiwhistIncrementalPagedSourceHttpCrawler {
    private static final String VERSION = "2";

    private static final String SEARCH_FORM_URL = "https://www.simap.ch/shabforms/COMMON/search/searchForm.jsf";

    private static final String DATE_SPAN_RADIO_XPATH = "//input[@id='TIMESPAN_VARIABLE']";
    private static final String DATE_FROM_INPUT_XPATH = "//input[@id='STAT_TM_1']";
    private static final String DATE_TO_INPUT_XPATH = "//input[@id='STAT_TM_2']";
    private static final String SEARCH_BUTTON_XPATH = "//input[@type='submit']";
    private static final String NEXT_BUTTON_XPATH = "//a[text()='»']";

    private static final LocalDate OLDEST_NOTICE_DATE = LocalDate.of(2007, 5, 23);
    private static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofPattern("dd.MM.yyyy");

    private static final int EXCEPTIONS_COUNT_LIMIT = 50;

    /**
     * Default constructor, initializes everything important i.e. urls, xpath of elements etc.
     */
    public SimapTenderCrawler() {
        super();
        getWebClient().getOptions().setJavaScriptEnabled(false);
    }

    @Override
    public HtmlPage extractDetailsFromPage(final HtmlPage page) {
        @SuppressWarnings("unchecked") final List<HtmlTableRow> noticeTableRows = (List<HtmlTableRow>) page
                .getByXPath(
                        "//table[@id='resultList']/tbody/tr");

        assert !noticeTableRows.isEmpty() : "Table should have at least one row even if there is no result!";
        noticeTableRows.remove(0);  // first row is the head of table (if some result exists)

        for (HtmlTableRow noticeTableRow : noticeTableRows) {
            // Sometimes there are rows containing info about tender lots. These rows should be skipped. They are
            // recognized by number of cells.
            if (noticeTableRow.getCells().size() == 2) {
                continue;
            }

            final HtmlElement noticeLink = noticeTableRow.getFirstByXPath("td[4]/a");
            HtmlPage noticePage;
            int exceptionsCount = 0;
            while (true) {
                try {
                    noticePage = noticeLink.click();
                    break;
                } catch (ConnectException e) {
                    logger.error("Failing ConnectException code exception during notice link click.", e);
                    if (++exceptionsCount > EXCEPTIONS_COUNT_LIMIT) {
                        throw new UnrecoverableException("Unable to crawl page. Notice link does not work.", e);
                    }
                } catch (FailingHttpStatusCodeException e) {
                    logger.error("Failing HttpStatus code exception during notice link click.", e);
                    if (++exceptionsCount > EXCEPTIONS_COUNT_LIMIT) {
                        throw new UnrecoverableException("Unable to crawl page. Notice link does not work.", e);
                    }
                } catch (ClassCastException e) {
                    logger.error("Unexpected class cast exception during notice link click.", e);
                    if (++exceptionsCount > EXCEPTIONS_COUNT_LIMIT) {
                        throw new UnrecoverableException("Unable to crawl page. Notice link does not work.", e);
                    }
                } catch (IOException e) {
                    logger.error("Crawling failed for page #{} on url {} with exception {}", getCurrentPageNumber(),
                            getCurrentPageUrl(), e);
                    throw new UnrecoverableException("Crawling failed.", e);
                }
            }

            HtmlTableCell dateCell = noticeTableRow.getCell(0);
            HtmlTableCell typeCell = noticeTableRow.getCell(2);
            @SuppressWarnings("unchecked") final HashMap<String, Object> metadata = new HashMap();
            metadata.put("date", dateCell.getTextContent());
            metadata.put("typeOrDeadline", typeCell.getTextContent());

            createAndPublishMessage(noticePage.getUrl().toString(), noticePage.getWebResponse().getContentAsString(),
                    metadata);
        }
        return page;
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
                final HtmlRadioButtonInput dateSpanRadioButtonInput = actualPage.getFirstByXPath(
                        DATE_SPAN_RADIO_XPATH);
                dateSpanRadioButtonInput.setChecked(true);
                String localDateString = date.format(FORMATTER);
                final HtmlTextInput dateFromInput = actualPage.getFirstByXPath(DATE_FROM_INPUT_XPATH);
                dateFromInput.setText(localDateString);
                final HtmlTextInput dateToInput = actualPage.getFirstByXPath(DATE_TO_INPUT_XPATH);
                dateToInput.setText(localDateString);

                final HtmlElement searchButton = actualPage.getFirstByXPath(SEARCH_BUTTON_XPATH);
                try {
                    return searchButton.click();
                } catch (ConnectException e) {
                    logger.error("Failing ConnectException code exception during search button click.", e);
                    if (++exceptionsCount > EXCEPTIONS_COUNT_LIMIT) {
                        throw new UnrecoverableException("Unable to crawl page. Seach button does not work.", e);
                    }
                } catch (FailingHttpStatusCodeException e) {
                    logger.error("Failing HttpStatus code exception during search button click.", e);
                    if (++exceptionsCount > EXCEPTIONS_COUNT_LIMIT) {
                        throw new UnrecoverableException("Unable to crawl page. Search button not work.", e);
                    }
                } catch (ClassCastException e) {
                    logger.error("Unexpected class cast exception during search button click.", e);
                    if (++exceptionsCount > EXCEPTIONS_COUNT_LIMIT) {
                        throw new UnrecoverableException("Unable to crawl page. Search button does not work.", e);
                    }
                } catch (IOException e) {
                    logger.error("Crawling failed for page #{} on url {} with exception {}", getCurrentPageNumber(),
                            getCurrentPageUrl(), e);
                    throw new UnrecoverableException("Crawling failed.", e);
                }
            }
        } catch (IOException e) {
            logger.error("Getting result for date {} failed.", date, e);
            throw new UnrecoverableException("Unable to get result page", e);
        }
    }

    @Override
    public HtmlPage getNextPage(final HtmlPage actualPage) {
        int exceptionsCount = 0;
        while (true) {
            try {
                return CrawlerUtils.clickElement(actualPage, NEXT_BUTTON_XPATH);
            } catch (FailingHttpStatusCodeException e) {
                logger.error("Failing HttpStatus code exception during get next page button click.", e);
                if (++exceptionsCount > EXCEPTIONS_COUNT_LIMIT) {
                    throw new UnrecoverableException("Unable to crawl page. Next page button not work.", e);
                }
            } catch (ClassCastException e) {
                logger.error("Unexpected class cast exception during get next page button click.", e);
                if (++exceptionsCount > EXCEPTIONS_COUNT_LIMIT) {
                    throw new UnrecoverableException("Unable to crawl page. Next page button does not work.", e);
                }
            }
        }
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
    protected ChronoUnit getIncrementUnit() {
        return ChronoUnit.DAYS;
    }
}
