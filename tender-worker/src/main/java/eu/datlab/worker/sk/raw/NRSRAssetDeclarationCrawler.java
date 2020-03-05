package eu.datlab.worker.sk.raw;

import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlOption;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import com.gargoylesoftware.htmlunit.html.HtmlSelect;
import com.gargoylesoftware.htmlunit.html.HtmlTableCell;
import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.worker.Message;
import eu.dl.worker.raw.BaseHttpCrawler;

import java.io.IOException;
import java.util.List;

/**
 * Searches all public officials profiles on http://www.nrsr.sk/web/Default.aspx?sid=vnf/zoznam&ViewType=1 for asset
 * declaration.
 *
 * @author Tomas Mrazek
 */
public final class NRSRAssetDeclarationCrawler extends BaseHttpCrawler {
    private static final String VERSION = "3";
    private static final String SOURCE_URL = "http://www.nrsr.sk/web/";
    private static final String PUBLIC_OFFICIALS_LIST_URL = SOURCE_URL + "Default.aspx?sid=vnf/zoznam&ViewType=1";
    private static final int HTTP_SUCCESS_CODE = 200;
    /**
     * Waiting interval in milliseconds between two attempts to receive correct year asset declaration page.
     */
    private static final int SLEEP_INTERVAL = 1000;
    /**
     * Maximal number of attempts to receive the correct year asset declaration page.
     */
    private static final int MAX_ATTEMPTS = 10;

    @Override
    protected void doWork(final Message message) {
        final HtmlPage page;
        try {
            page = getWebClient().getPage(PUBLIC_OFFICIALS_LIST_URL);
            // check for valid response
            if (page.getWebResponse().getStatusCode() != HTTP_SUCCESS_CODE) {
                logger.error("Received non-200 status code while trying to read from {}.", page.getUrl());
                throw new UnrecoverableException("Crawling failed due to non-200 HTTP response.");
            }
        } catch (IOException e) {
            logger.error("Crawling of public officials list from {} failed with exception {}",
                    PUBLIC_OFFICIALS_LIST_URL, e);
            throw new UnrecoverableException("Crawling of public officials list failed.", e);
        }

        //first div contains only navigation links, skip it
        final List<HtmlAnchor> publicOfficials = page.getByXPath(
                "//div[@class='form']/div/div[position()>1]/a[contains(@href,'Default.aspx?')]");

        for (HtmlAnchor anchor : publicOfficials) {
            logger.info("Crawling of public official {} starts.", anchor.getTextContent());
            try {
                processDetail(anchor.click());
            } catch (IOException e) {
                logger.error("Crawling of detail from {} failed with exception {}",
                        SOURCE_URL + anchor.getHrefAttribute(), e);
                throw new UnrecoverableException("Crawling of detail failed.", e);
            }
        }
    }

    @Override
    protected String getVersion() {
        return VERSION;
    }

    /**
     * For each year sends page to downloader.
     *
     * @param detailPage
     *         detail page of the public official
     */
    private void processDetail(final HtmlPage detailPage) {
        HtmlPage yearAssetDeclarationPage = detailPage;

        final HtmlSelect select = yearAssetDeclarationPage.getFirstByXPath(
                "//select[@id='_sectionLayoutContainer_ctl01_OznameniaList']");

        for (int i = 1; i <= select.getOptionSize(); i++) {
            final HtmlOption option = yearAssetDeclarationPage.getFirstByXPath(
                    "//select[@id='_sectionLayoutContainer_ctl01_OznameniaList']//option[position()=" + i + "]");

            try {
                yearAssetDeclarationPage = getAssetDeclarationYearPage(option);
                if (yearAssetDeclarationPage != null) {
                    sendYearAssetDeclarationToDownloader(yearAssetDeclarationPage);
                    logger.info("Asset declaration for year {} acquired and sent to downloader.", option.getText());
                } else {
                    logger.error("Asset declaration for year {} not downloaded.", option.getText());
                    break;
                }
            } catch (IOException | InterruptedException e) {
                logger.error("Downloading of asset declaration for year {} failed with exception {}.",
                        option.getText(), e);
            }
        }
    }

    /**
     * Sends given year asset declaration detail to downloader.
     *
     * @param yearAssetDeclarationPage
     *         detail page for year
     */
    private void sendYearAssetDeclarationToDownloader(final HtmlPage yearAssetDeclarationPage) {
        createAndPublishMessage(yearAssetDeclarationPage.getUrl().toString(), yearAssetDeclarationPage.asXml());
    }

    /**
     * Loads year asset declaration page by clicking on given option element. Waits for correct page. Correct page
     * contains in 'oznámenie za rok:' cell the same year as value of the selected option.
     *
     * @param option
     *         option from year select
     *
     * @return correct asset declaration page or null
     * @throws IOException
     *         In case that option click fails
     * @throws InterruptedException
     *         If any thread has interrupted the current thread. The interrupted status of the current thread is
     *         cleared
     *         when this exception is thrown.
     */
    private HtmlPage getAssetDeclarationYearPage(final HtmlOption option) throws IOException, InterruptedException {
        HtmlPage yearAssetDeclarationPage = option.click();
        final String year = option.getTextContent();

        int attempts = MAX_ATTEMPTS;
        while (attempts > 0 && !year.equals(getAssetDeclarationYear(yearAssetDeclarationPage))) {
            yearAssetDeclarationPage = option.click();
            Thread.sleep(SLEEP_INTERVAL);
            attempts--;
        }

        if (!year.equals(getAssetDeclarationYear(yearAssetDeclarationPage))) {
            return null;
        }

        return yearAssetDeclarationPage;
    }

    /**
     * Returns year of an asset declaration from page content.
     *
     * @param page
     *         page of asset declaration
     *
     * @return asset declaration year or null if not found
     */
    private String getAssetDeclarationYear(final HtmlPage page) {
        final HtmlTableCell cell = page.getFirstByXPath(
                "//table[@class='oznamenie_table']//td[contains(text(),'oznámenie za rok:')]");

        return (cell == null ? null : cell.getParentNode().getNextSibling().getTextContent());
    }

    @Override
    protected TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }
}
