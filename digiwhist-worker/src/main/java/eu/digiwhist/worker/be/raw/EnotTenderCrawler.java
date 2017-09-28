package eu.digiwhist.worker.be.raw;

import java.io.IOException;
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import com.gargoylesoftware.htmlunit.html.HtmlTableCell;
import com.gargoylesoftware.htmlunit.html.HtmlTableRow;

import eu.digiwhist.worker.raw.BaseDigiwhistIncrementalPagedSourceHttpCrawler;
import eu.dl.core.UnrecoverableException;
import eu.dl.worker.raw.utils.CrawlerUtils;
import eu.dl.worker.utils.http.URLUtils;

/**
 * This class is searching https://enot.publicprocurement.be for tenders xml and publishes appropriate RabbitMQ
 * messages. Creates messages for html detail page and all xml documents from "Notices" section table in detail.
 *
 * @author Tomas Mrazek
 */
public final class EnotTenderCrawler extends BaseDigiwhistIncrementalPagedSourceHttpCrawler {
    private static final String VERSION = "2";

    private static final String SOURCE_DOMAIN = "https://enot.publicprocurement.be/enot-war/";

    private static final String FIRST_PAGE_URL_PATTERN = SOURCE_DOMAIN + "searchNotice.do" +
            "?marketPlaceType=both&useWorkingOrganisationId=false&selectAllChildren=true&noticeStatus=0" +
            "&allLanguages=true&pageSize=125&publicationDateBDAFrom=%s&publicationDateBDATo=%s";

    private static final String NEXT_BUTTON_XPATH = "//span[@class='pagelinks']/a[text()='Next']";

    /**
     * Pattern of direct xml link.
     */
    private static final String XML_URL_PATTERN = SOURCE_DOMAIN + "getNoticeXml.do" +
            "?noticeVersion=%s&isErratum=%s&noticeId=%s&languageCharset=%s";
    /**
     * Pattern of html detail link.
     */
    private static final String HTML_URL_PATTERN = SOURCE_DOMAIN + "preViewNotice.do?noticeId=%s";
    /**
     * Regex for noticeId, isErratum and noticeVersion param extraction.
     */
    private static final String XML_PARAMS_REGEX = "menuXml(\\d+)(\\d{1})(\\d{1})";
    /**
     * Group index of noticeId value in regex matches.
     */
    private static final int XML_PARAM_NOTICE_ID = 1;
    /**
     * Group index of noticeVersion value in regex matches.
     */
    private static final int XML_PARAM_NOTICE_VERSION = 2;
    /**
     * Group index of isErratum value in regex matches.
     */
    private static final int XML_PARAM_IS_ERRATUM = 3;
    /**
     * Date of the oldest notice.
     */
    private static final LocalDate OLDEST_NOTICE_DATE = LocalDate.of(2009, 6, 15);
    /**
     * String format pattern of crawled url date parameter value.
     */
    private static final String URL_DATE_PARAMETER_PATTERN = "%02d%%2F%02d%%2F%4d";

    @Override
    protected HtmlPage getSearchResultsStartPageForDate(final LocalDate incrementDate) {
        final String urlDateParameterValue = String.format(URL_DATE_PARAMETER_PATTERN, incrementDate.getDayOfMonth(),
                incrementDate.getMonthValue(), incrementDate.getYear());
        final String searchResultsForDateUrl = String.format(FIRST_PAGE_URL_PATTERN, urlDateParameterValue,
                urlDateParameterValue);
        try {
            return getWebClient().getPage(searchResultsForDateUrl);
        } catch (IOException e) {
            logger.error("Crawling failed for page with url {} with exception {}", getCurrentPageUrl(), e);
            throw new UnrecoverableException("Unable to crawl page", e);
        }
    }

    @Override
    protected LocalDate getDefaultStartDate() {
        return OLDEST_NOTICE_DATE;
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
    public HtmlPage extractDetailsFromPage(final HtmlPage page) {
        final List<HtmlAnchor> nodes = (List<HtmlAnchor>) page.getByXPath("//table[@id='notice']/tbody/tr/td[1]/a");

        for (final HtmlAnchor detail : nodes) {
            try {
                HtmlPage detailPage = detail.click();
                //message for html detail
                createAndPublishMessage(
                        String.format(HTML_URL_PATTERN, URLUtils.getUrlParameter(detailPage.getUrl(), "noticeId")));

                //message for each xml document in "Notices" section table
                List<HtmlTableRow> tableRows = (ArrayList) detailPage.getByXPath("//table[@id='noticeVer']/tbody/tr");

                for (HtmlTableRow row : tableRows) {
                    final String[] langs = getFileLangs(row);

                    if (langs.length == 0) {
                        continue;
                    }

                    //download only first language document variant
                    final String downloadUrl = getLangXmlUrl(row, langs[0]);
                    if (downloadUrl != null) {
                        createAndPublishMessage(downloadUrl);
                    } else {
                        logger.warn(
                                "Generating of direct xml url fails for {}-th document in 'Notices' section of " +
                                        "detail" + " page {}.",
                                (row.getIndex() + 1), detailPage.getUrl());
                    }
                }
            } catch (final Exception e) {
                logger.error("Crawling failed for page #{} on url {} with exception {}", getCurrentPageNumber(),
                        getCurrentPageUrl(), e);
                throw new UnrecoverableException("Crawling failed.", e);
            }
        }
        return page;
    }

    @Override
    public boolean isPageValid(final HtmlPage page) {
        return true;
    }

    @Override
    protected ChronoUnit getIncrementUnit() {
        return ChronoUnit.DAYS;
    }

    /**
     * Extracts supported languages for files in given table row. Langs are palced in third column of table row. Value
     * has form [<lang>, <lang>,...].
     *
     * @param row
     *         table row
     *
     * @return array of languages
     */
    private String[] getFileLangs(final HtmlTableRow row) {
        final HtmlTableCell langCell = row.getFirstByXPath("td[position()=3]");

        return langCell.getTextContent().replace("]", "").replace("[", "").replace(" ", "").split(",");
    }

    /**
     * Generates direct download link for xml file in given table row. Xml link parameters are extracted from parameter
     * of showPromptClose function called in onclick attribute of xml file download link.
     * Parameter form is menuXml<noticeId><noticeVersion><isErratum>.
     *
     * @param row
     *         table row
     * @param lang
     *         lang of file
     *
     * @return direct xml download link or null if attempt to extract paramters noticeId, isErratum and noticeVersion
     * fails.
     */
    private String getLangXmlUrl(final HtmlTableRow row, final String lang) {
        final HtmlAnchor link = row.getFirstByXPath("td[last()]//img[@title='Download XML notice']/ancestor::a");

        if (link == null) {
            return null;
        }

        String downloadLinkOnClick = link.getAttribute("onclick");
        Pattern pattern = Pattern.compile(XML_PARAMS_REGEX);
        Matcher matcher = pattern.matcher(downloadLinkOnClick);

        if (matcher.find()) {
            return String.format(XML_URL_PATTERN, matcher.group(XML_PARAM_NOTICE_VERSION),
                    matcher.group(XML_PARAM_IS_ERRATUM), matcher.group(XML_PARAM_NOTICE_ID), lang);
        }

        return null;
    }
}
