package eu.datlab.worker.es.raw;

import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import com.gargoylesoftware.htmlunit.html.HtmlSubmitInput;
import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.worker.Message;
import eu.dl.worker.raw.BasePagedSourceHttpCrawler;
import eu.dl.worker.raw.utils.CrawlerUtils;

import java.io.IOException;
import java.util.List;

/**
 * This class is searching https://contrataciondelestado.es for contracting
 * authority details and save them. Combines functionality of crawler and
 * downloader.
 *
 * @author Tomas Mrazek
 */
public final class PCEContractingAuthorityCrawler extends BasePagedSourceHttpCrawler {
    private static final String VERSION = "1";
    private static final String SOURCE_DOMAIN = "https://contrataciondelestado.es/";
    private static final String START_PAGE_URL = SOURCE_DOMAIN +
            "wps/portal/!ut/p/b1/vZTbcpswEIafJQ-Q0UriIF8STgKDIAJhw43HxU4G7Nht4zY1T" +
            "x_F04wn7oR2pk2lK0mf9t-DVqhBNQGCJxMMBkVz1OyW37v75aHb75bbl3VjLZzKv3WiCY" +
            "VQ5j6QG2lbRpBRAKKBWgPwznDgdN_wM9cNOAHwUwqE2yHzqQuQm2iGav7TyAj1xkhoKgdI" +
            "qcoqlQFA-OqESV2jiqvcKqIQIOKBlyhs6nPrz4IYEXijzwrqAUk8pSx-oi_v_wr8Rn-Gmh" +
            "MyFsEJGMvzuMgr8K6TBAm-f1ijYr1DtUbtsy1WSF2P2DQBRIQBDFSiORiLoj9-jobNIPvh" +
            "lqR9MQhPUSgrUW5sKdQGF4dYlJ7CuFdE9E0qfL3Rt0SsHP5lV2fdys0rT0WMB7Da4zypas" +
            "z8x8cCcF6mdjrgNBq6H3FVzp_KFr5Wcy8bjod4aA_psWoxy-XADJZ7dxygvcbToPWdeMmX" +
            "gXN1GQQkmQVETnNpFky_MfPvg_DyVSXVjeN-Wy9iuBTMphOmK0ATIhjVzfXhgsyO3ZeSG1" +
            "Fh6VWMP0jw3KdJot8FNXnmFwSHFvnfKbX_uWCMmvvt_pP--WYRemi2ybHruuuN5E_i7jxT" +
            "5-oZmsahSg!!/dl4/d5/L2dBISEvZ0FBIS9nQSEh/pw/Z7_AVEQAI930GRPE02BR764FO3" + "0G0/act/id=0/p=javax.servlet"
            + ".include.path_info=QCPjspQCPlistPerfilesQC" + "PAdminAFPListPerfPortletAppView.jsp/294418347670/-/";

    private static final String NEXT_BUTTON_XPATH = "//input[@id='viewns_Z7_AVEQAI930GRPE02BR7" +
            "64FO30G0_:listaperfiles:siguienteLink']";

    private static final String SEARCH_BUTTON_XPATH = "//input[@id='viewns_Z7_AVEQAI930GRPE02" +
            "BR764FO30G0_:listaperfiles:botonbuscar']";

    @Override
    public HtmlPage extractDetailsFromPage(final HtmlPage page) {
        final List<HtmlAnchor> nodes = page.getByXPath("//table[@id='tableBusquedaPerfilContratante']/tbody/tr/td[1]/a");

        HtmlPage actualPage = page;

        for (int i = 1; i <= nodes.size(); i++) {
            final HtmlAnchor detailLink = actualPage.getFirstByXPath(
                    "//table[@id='tableBusquedaPerfilContratante']/tbody/tr[" + i + "]/td[1]/a");
            final String url = detailLink.getHrefAttribute();

            try {
                // get detail page
                final HtmlPage detailPage = detailLink.click();

                createAndPublishMessage(url, detailPage.getWebResponse().getContentAsString());

                // get back from detail page to list page
                final HtmlSubmitInput backButton = detailPage.getFirstByXPath(
                        "//*[@id='viewns_Z7_AVEQAI930GRPE02BR764FO30G0_:perfilComp:idBotonVolver']");

                actualPage = backButton.click();
            } catch (IOException e) {
                logger.error("Crawling failed when checking detail page {} with exception {}", url, e);
                throw new UnrecoverableException("Crawling failed.", e);
            }
        }
        return actualPage;
    }

    @Override
    protected HtmlPage getSearchResultsStartPage(final Message message) {
        try {
            logger.debug("Getting the first page to start crawling from.");
            HtmlPage searchPage = getWebClient().getPage(START_PAGE_URL);
            return CrawlerUtils.clickElement(searchPage, SEARCH_BUTTON_XPATH);
        } catch (final Exception e) {
            logger.error("Crawling failed for start page {} with exception {}", START_PAGE_URL, e);
            throw new UnrecoverableException("Crawling failed.", e);
        }
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
    protected TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }
}
