package eu.datlab.worker.it.raw;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.RawDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.raw.Raw;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.Message;
import eu.dl.worker.raw.downloader.BaseDownloader;
import eu.dl.worker.raw.utils.DownloaderUtils;
import org.jsoup.Connection;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.io.IOException;
import java.io.StringReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

/**
 * Downloads xmls for tenders.
 */
public final class ANACTenderDownloader extends BaseDownloader {
    private static final String VERSION = "1.1";

    @Override
    protected boolean skipExisting(final Message message) {
        return false;
    }

    /**
     * Creates xml document from string.
     * @param data xml data in string
     * @return xml document
     */
    private Document stringToXmlDocument(final String data) {
        DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
        DocumentBuilder dBuilder = null;
        Document doc = null;
        try {
            dBuilder = dbFactory.newDocumentBuilder();
            doc = dBuilder.parse(new InputSource(new StringReader(
                    // to avoid "Content is not allowed in prolog"
                    data.trim().replaceFirst("^([\\W]+)<", "<")
            )));
            doc.getDocumentElement().normalize();
        } catch (ParserConfigurationException | SAXException | IOException e) {
            e.printStackTrace();
        }
        return doc;
    }

    /**
     * Extracts raw data for tenders from xml document.
     * @param data xml document
     * @param url  url of document
     * @return raw data for tenders
     */
    private List<Raw> saveLottoXml(final String data, final String url) {
        List<Raw> rawData = new ArrayList<>();
        RawData raw = new RawData();
        raw.setSourceData(data);
        try {
            raw.setSourceUrl(new URL(url));
        } catch (MalformedURLException e) {
            e.printStackTrace();
        }
        rawData.add(raw);
        return rawData;
    }


    /**
     * Extracts raw data for tenders from xml document containing
     * links to other xml documents containing data about tenders.
     * @param data xml documents with links to xml documents
     * @return raw data for tenders
     */
    private List<Raw> parseIndiceXml(final Document data) {
        List<Raw> rawData = new ArrayList<>();
        Node indiceRoot = data.getElementsByTagName("indice").item(0);
        NodeList elements = indiceRoot.getChildNodes(); // hope there are dataset id = ...
        String tenderDocUrlStr = null;
        // list of nodes with info about documents for every lot
        for (int i = 0; i < elements.getLength(); i++) {
            NodeList fields = elements.item(i).getChildNodes();
            URL tenderDocUrl = null;
            for (int j = 0; j < fields.getLength(); j++) {
                if (fields.item(j).getNodeName().equals("linkDataset")) {
                    // link on document containing info about tender
                    tenderDocUrlStr = fields.item(j).getChildNodes().item(0).getNodeValue();
                    // download only xml
                    if (!tenderDocUrlStr.endsWith(".xml")) {
                        continue;
                    }
                    try {
                        tenderDocUrl = new URL(tenderDocUrlStr);
                    } catch (final MalformedURLException ex) {
                        logger.error("Unable to download from malformed URL {}", tenderDocUrlStr);
                        throw new UnrecoverableException("Unable to download data because of malformed url", ex);
                    }
                    break;
                }
            }
            if (tenderDocUrl != null) {
                final Connection.Response response = DownloaderUtils.getUrlResponse(tenderDocUrlStr);
                logger.info("Downloaded tender data from {}", tenderDocUrlStr);
                // parse every downloaded file
                rawData.addAll(saveLottoXml(DownloaderUtils.getResponseBody(response), tenderDocUrlStr));
            }
        }
        return rawData;
    }

    @Override
    public List downloadAndPopulateRawData(final Message message) {
        List<Raw> rawData = new ArrayList<>();
        final String sourceDataUrl = message.getValue("url");
        // download only xml
        if (!sourceDataUrl.endsWith(".xml")) {
            return new ArrayList<>();
        }

        final Connection.Response response = DownloaderUtils.getUrlResponse(sourceDataUrl);
        logger.info("Downloaded publisher data from {} ", sourceDataUrl);
        String data = DownloaderUtils.getResponseBody(response);

        if (data != null) {
            if (data.contains("lotto")) {
                // file already contains data about tenders
                rawData = saveLottoXml(data, sourceDataUrl);
            } else if (data.contains("indice")) {
                // file contains links to files containing data about tenders
                Document doc = stringToXmlDocument(data);
                rawData = parseIndiceXml(doc);
            }
        }
        return rawData;
    }

    @Override
    public String getVersion() {
        return VERSION;
    }

    @Override
    protected void postProcess(final Raw raw) {
    }

    @Override
    public RawDAO getRawDataDao() {
        return DAOFactory.getDAOFactory().getRawTenderDAO(getName(), getVersion());
    }

    @Override
    protected TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }

}
