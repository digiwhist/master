package eu.datlab.worker.in.raw;

import com.gargoylesoftware.htmlunit.WebClient;
import com.gargoylesoftware.htmlunit.html.HtmlForm;
import com.gargoylesoftware.htmlunit.html.HtmlInput;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import com.gargoylesoftware.htmlunit.html.HtmlSpan;
import com.gargoylesoftware.htmlunit.html.HtmlTableRow;
import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.dl.core.UnrecoverableException;
import eu.dl.worker.Message;
import eu.dl.worker.MessageFactory;
import eu.dl.worker.raw.utils.CrawlerUtils;
import org.apache.commons.lang3.StringUtils;
import org.mariuszgromada.math.mxparser.Expression;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.format.DateTimeParseException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;


/**
 * Utils class for India crawlers.
 */
public final class CPPPCrawlerUtils {

    private static final Logger logger = LoggerFactory.getLogger(CPPPCrawlerUtils.class.getName());

    private static final DateTimeFormatter PUBLICATION_DATE_FORMAT = new DateTimeFormatterBuilder()
        .parseCaseInsensitive()
        .appendPattern("d-MMM-uuuu h:m a")
        .toFormatter(new Locale("en", "EN"));

    private static final String USER_AGENT = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_6) AppleWebKit/537.36 " +
        "(KHTML, like Gecko) Chrome/68.0.3440.84 Safari/537.36";

    private static final WebClient webClient;
    static {
        webClient = new WebClient();
        webClient.getOptions().setUseInsecureSSL(true);
        webClient.getOptions().setThrowExceptionOnScriptError(false);
        webClient.getOptions().setThrowExceptionOnFailingStatusCode(false);
        webClient.addRequestHeader("Referer", PublicationSources.IN_CPPP);
        webClient.addRequestHeader("User-Agent", USER_AGENT);
    }

    /**
     * Suppress default constructor.
     */
    private CPPPCrawlerUtils() {
    }

    /**
     * @param page
     *      current page
     * @param n
     *      number of required page
     * @return n-th page
     */
    public static HtmlPage getNthPage(final HtmlPage page, final int n) {
        // remove page query parameter from the end of url
        String url = page.getUrl().toString().split("\\?")[0];

        // page number in query parameter is 0-based
        return getCAPTCHAProtectedPage(url + "?page=" + (n - 1));
    }

    /**
     * Extracts details from given page and return list of RabbitMQ messages for downloader. Publication date is added to message's
     * metadata under key publicationDate.
     *
     * @param page
     *         current page
     * @param dateColumn
     *         index of date column (0-based)
     * @param linkColumn
     *         index of detail link column (0-based)
     * @return list of messages
     */
    public static List<Message> extractDetailsFromPage(final HtmlPage page, final int dateColumn, final int linkColumn) {
        List<Message> messages = new ArrayList<>();

        List<HtmlTableRow> rows = getRecords(page);

        for (HtmlTableRow r : rows) {
            LocalDate published = null;
            try {
                String publishedText = r.getCell(dateColumn).asText();
                if (publishedText != null) {
                    published = LocalDate.parse(publishedText, PUBLICATION_DATE_FORMAT);
                }
            } catch (DateTimeParseException e) {
                logger.warn("Unable to parse publication date from string {}", published);
            }

            String hrefAttr = r.getCell(linkColumn).getFirstElementChild().getAttribute("href");
            if (hrefAttr == null) {
                logger.warn("Unable to get href attribute of detail link");
                continue;
            }

            String url = PublicationSources.IN_CPPP + StringUtils.stripStart(hrefAttr, "/");
            HtmlPage detail = getCAPTCHAProtectedPage(url);

            if (detail.getFirstByXPath("//div[@id='edit-t-fullview']//td[contains(text(), 'Invalid parameter')]") != null) {
                logger.error("Invalid request for tender detail on {}", url);
                throw new UnrecoverableException("Invalid request for tender detail");
            }

            HashMap<String, Object> metaData = new HashMap<>();
            metaData.put("publicationDate", published);
            messages.add(MessageFactory.getMessage()
                .setValue("url", url)
                .setValue("sourceData", detail.getWebResponse().getContentAsString())
                .setMetaData(metaData));
        }

        return messages;
    }

    /**
     * @param page
     *      page to be searched
     * @return list of data table rows or empty list
     */
    public static List<HtmlTableRow> getRecords(final HtmlPage page) {
        List<HtmlTableRow> rows = page.getByXPath("//table[@id='edit-table']/tbody/tr");
        // for the empty result the table with one row including text 'No Records Found' is displayed
        return (rows == null || rows.isEmpty() || rows.get(0).asText().matches("(?i)No (Records|Data) Found"))
            ? Collections.emptyList() : rows;
    }

    /**
     * Attempts to load protected page.
     *
     * @param url
     *      requested url
     * @return protected page
     */
    public static HtmlPage getCAPTCHAProtectedPage(final String url) {
        try {
            HtmlPage actualPage = getWebClient().getPage(url);

            return resolveCAPTCHA(actualPage, url);
        } catch (IOException e) {
            logger.error("Unable to CAPTCHA protected page with url {} with exception {}", url, e);
            throw new UnrecoverableException("Unable to crawl page", e);
        }
    }

    /**
     * Check whether the given page includes CAPTCHA form. For CAPTCHA page it attempts to resolve expression and submits form. For
     * correctly filled answer loads requested URL.
     *
     * @param page
     *      page to be
     * @param requestedUrl
     *      requested URL
     * @return requested page
     */
    public static HtmlPage resolveCAPTCHA(final HtmlPage page, final String requestedUrl) {
        HtmlForm form = getCAPTCHAForm(page);
        // check whether the page is CAPTCHA page
        if (form == null) {
            // page isn't CAPTCHA page
            return page;
        }

        HtmlSpan questionNode = form.getFirstByXPath("//span[@class='field-prefix']");
        if (questionNode == null) {
            logger.error("Unable to read CAPTCHA question field");
            throw new UnrecoverableException("Unable to read CAPTCHA question field");
        }

        HtmlInput answerNode = form.getInputByName("captcha_response");
        if (answerNode == null) {
            logger.error("Unable to find CAPTCHA answer field");
            throw new UnrecoverableException("Unable to find CAPTCHA answer field");
        }

        // question string includes character '='
        String question = questionNode.asText();
        Expression captcha = new Expression(question.replace("=", "").trim());
        int answer = (int) captcha.calculate();

        answerNode.setValueAttribute(String.valueOf(answer));

        // submitting of the CAPTCHA form redirects to the homepage if the answer is correct, otherwise redirects to CAPTCHA page again.
        HtmlPage homepage = CrawlerUtils.clickElement(page, "//input[@type='submit' and @value='Submit']");
        if (getCAPTCHAForm(homepage) != null) {
            logger.error("CAPTCHA has filled incorrectly. Question: {}, Answer: {}", question, answer);
            throw new UnrecoverableException("CAPTCHA has filled incorrectly");
        }

        logger.info("CAPTCHA resolved. Question: {}, Answer: {}", question, answer);

        try {
            return getWebClient().getPage(requestedUrl);
        } catch (IOException e) {
            logger.error("Unable to load requested url {} after CAPTCHA submitting", requestedUrl, e);
            throw new UnrecoverableException("Unable to load requested url", e);
        }
    }

    /**
     * @param page
     *      page to be searched
     * @return CAPTCHA form if exists, otherwise NULL
     */
    private static HtmlForm getCAPTCHAForm(final HtmlPage page) {
        return  page.getFirstByXPath("//form[@id='verify-captcha-form']");
    }

    /**
     * @return web client instance
     */
    public static WebClient getWebClient() {
        return webClient;
    }
}
