package eu.datlab.worker.cl.raw;

import com.gargoylesoftware.htmlunit.WebClient;
import com.gargoylesoftware.htmlunit.html.DomElement;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.core.RecoverableException;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.RawDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.raw.Raw;
import eu.dl.worker.Message;
import eu.dl.worker.raw.downloader.BaseDownloader;
import eu.dl.worker.utils.NetworkUtils;
import org.apache.commons.codec.binary.Hex;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.zip.GZIPOutputStream;

/**
 * Downloader for Chile.
 *
 * @param <T>
 */
public final class MPTenderDownloader<T extends Raw> extends BaseDownloader<T> {
    private static final String VERSION = "1";

    private static final String BASE_URL = "https://www.mercadopublico.cl";
    private static final String PAGE_INVALID = "procurement/Includes/images/robot.jpg";

    private WebClient webClient;

    /**
     * Create webclient for downloader.
     */
    public MPTenderDownloader() {
        getNewWebclient();

        // check whether TOR should be started
        if (config.getParam(getName() + ".torEnabled") != null
                && config.getParam(getName() + ".torEnabled").equals("1")) {
            NetworkUtils.enableTorForHttp();
        }
    }

    @Override
    public List<T> downloadAndPopulateRawData(final Message message) {
        final T rawData = rawDao.getEmptyInstance();

        final String sourceDataUrl = message.getValue("url");

        if (sourceDataUrl != null) {
            // check if url is no malformed
            try {
                rawData.setSourceUrl(new URL(sourceDataUrl));
            } catch (final MalformedURLException ex) {
                logger.error("Malformed URL {}", sourceDataUrl);
                throw new UnrecoverableException("Unable to download data because of malformed url", ex);
            }

            // download detail page
            logger.info("Downloading detail page {}", sourceDataUrl);
            HtmlPage detailPage = getPage(sourceDataUrl);
            logger.info("Downloaded data from {}", sourceDataUrl);

            if (detailPage.getWebResponse().getContentAsString().contains(PAGE_INVALID)) {
                throw new Error("We are blocked!");
            }

            // download metadata
            @SuppressWarnings("unchecked") final HashMap<String, Object> metadata = new HashMap();

            // download historial metadata
            // check if link exist and is not disabled
            final DomElement historialLink = detailPage.getElementById("imgHistorial");
            if (historialLink != null && !historialLink.getAttribute("disabled").equals("disabled")) {
                // download the page
                logger.info("Downloading historial page.");
                final HtmlPage historialPage = getPage(BASE_URL + historialLink.getAttribute("href"));
                logger.info("Historial page downloaded.");

                // save data in metadata
                @SuppressWarnings("unchecked") final HashMap<String, Object> historialData = new HashMap();
                historialData.put("url", historialPage.getUrl().toString());
                historialData.put("body", historialPage.getWebResponse().getContentAsString());
                metadata.put("historial", historialData);
            } else {
                logger.info("No historial page for this tender.");
            }

            // download adjudicacion
            // check if link exist and is not disabled
            final DomElement adjudicacionLink = detailPage.getElementById("imgAdjudicacion");
            if (adjudicacionLink != null && !adjudicacionLink.getAttribute("disabled").equals("disabled")) {
                // download the page
                logger.info("Downloading adjudicacion page.");
                HtmlPage adjudicacionPage = getPage(BASE_URL + adjudicacionLink.getAttribute("href"));
                logger.info("Adjudicacion page downloaded.");

                // save the page to temporary metadata
                @SuppressWarnings("unchecked") final HashMap<String, Object> adjudicacionData = new HashMap();
                adjudicacionData.put("url", adjudicacionPage.getUrl().toString());
                adjudicacionData.put("body", adjudicacionPage.getWebResponse().getContentAsString());

                metadata.put("adjudicacion", adjudicacionData);
            } else {
                logger.info("No adjudicacion page for this tender.");
            }

            rawData.setSourceData(detailPage.getWebResponse().getContentAsString());
            rawData.setSourceDataMimeType(detailPage.getWebResponse().getContentType());
            rawData.setMetaData(metadata);
        } else {
            logger.error("Invalid url, url NULL");
            throw new UnrecoverableException("Invalid url, url NULL");
        }

        // return result
        return Collections.singletonList(rawData);
    }

    /**
     * @param url
     *      url of the page
     * @return html page
     */
    private HtmlPage getPage(final String url) {
        HtmlPage page;
        try {
            page = webClient.getPage(url);
        } catch (IOException e) {
            logger.error("Unable to open page {} bacause of", url, e);
            throw new RecoverableException("Unable to open page.", e);
        }

        int code = page.getWebResponse().getStatusCode();
        if (code < 200 || code > 299) {
            logger.error("Failing http status code exception: {} {} for {}", code, page.getWebResponse().getStatusMessage(), url);
            throw new UnrecoverableException("Failing http status code exception");
        }

        return page;
    }

    @Override
    public String getVersion() {
        return VERSION;
    }

    @Override
    public RawDAO getRawDataDao() {
        return DAOFactory.getDAOFactory().getRawTenderDAO(getName(), getVersion());
    }

    @Override
    protected TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }

    /**
     * Comppress and encode (to be savable to db) long string.
     *
     * @param str string to compress
     * @return String
     * @throws IOException IOException
     */
    private static String compress(final String str) throws IOException {
        if (str == null || str.length() == 0) {
            return null;
        }

        ByteArrayOutputStream out = new ByteArrayOutputStream();
        GZIPOutputStream gzip = new GZIPOutputStream(out);
        gzip.write(str.getBytes());
        gzip.close();

        return Hex.encodeHexString(out.toByteArray());
    }

    @Override
    protected void postProcess(final T raw) {
    }

    /**
     * A.
     */
    private void getNewWebclient() {
        webClient = new WebClient();
        webClient.getOptions().setUseInsecureSSL(true);
        webClient.getOptions().setThrowExceptionOnScriptError(false);
        webClient.getOptions().setTimeout(90000);
        webClient.getOptions().setThrowExceptionOnFailingStatusCode(false);
    }
}
