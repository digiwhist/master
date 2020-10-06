package eu.datlab.worker.pl.raw;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import eu.datlab.dataaccess.dao.DAOFactory;
import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.dl.core.RecoverableException;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.RawDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.raw.Raw;
import eu.dl.worker.Message;
import eu.dl.worker.raw.downloader.BaseDownloader;
import eu.dl.worker.raw.utils.DownloaderUtils;
import org.jsoup.Connection;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import static eu.datlab.worker.pl.parsed.UZPJsonTenderUtils.parseItems;
import static eu.datlab.worker.pl.parsed.UZPJsonTenderUtils.textValue;

/**
 * UZP Tender JSON downloader.
 * There are multiple sources to download from for one publication and there can be multiple publications downloaded in each url
 * gotten in message. We are searching for rest of data for each publication and downloading it to metadata in pattern below.
 *
 * metaData:
 *  - tenderId:
 *      - additionalData
 *
 *
 * @param <T> item to be downloaded
 */
public final class UZPJsonTenderDownloader<T extends Raw> extends BaseDownloader<T> {
    private static final String VERSION = "1";

    private static final int MAX_ATTEMPTS = 20;
    private static final Integer DOWNLOAD_TIMEOUT = 600000;

    private static final String SOURCE_DOMAIN = PublicationSources.PL_UZP_JSON;
    private static final String ADDITONAL_URL = SOURCE_DOMAIN + "/BZP_PublicWebService.asmx/";
    private static final String FIND_BY_GUID = "?_%sGuid=";

    private static final HashMap<String, String> ZP400 = new HashMap<>();

    static {
        ZP400.put("CRITERIA", "KryteriaDoZP400_JSON");
        ZP400.put("ATTACHMENTS", "ZalacznikiDoZP400_JSON");
    }

    private static final HashMap<String, String> ZP403 = new HashMap<>();

    static {
        ZP403.put("CONTRACTORS", "WykonawcyDoZP403_JSON");
        ZP403.put("ATTACHMENTS", "ZalacznikiDoZP403_JSON");
    }

    private static final HashMap<String, String> ZP408 = new HashMap<>();

    static {
        ZP408.put("CONTRACTORS", "UmowyDoZP408_JSON");
    }

    private static final HashMap<String, String> ZP409 = new HashMap<>();

    static {
        ZP409.put("CONTRACTORS", "WykonawcyDoZP409_JSON");
    }

    @Override
    protected boolean skipExisting(final Message message) {
        return false;
    }

    @Override
    public List<T> downloadAndPopulateRawData(final Message message) {
        final String sourceDataUrl = message.getValue("url");

        final List<T> result = new ArrayList<>();

        if (sourceDataUrl != null) {
            T rawData = downloadFromUrl(sourceDataUrl);

            JsonNode json = null;
            try {
                ObjectMapper mapper = new ObjectMapper();
                json = mapper.readTree(rawData.getSourceData().replaceAll("<.*>", ""));
            } catch (IOException ex) {
                logger.error("Unable to parse JSON from raw data");
                throw new UnrecoverableException("Unable to parse JSON from raw data", ex);
            }


            final List<JsonNode> rawTenders = parseItems(json.findValue("Table"));

            final String formType = (String) message.getMetaData().get("formType");
            final HashMap<String, Object> allMetaData = message.getMetaData();

            for (JsonNode rawTender : rawTenders) {
                final String tenderGuid = textValue(new String[]{"GuidId", "Guid" + formType}, rawTender);

                if (formType.equals("ZP400")) {
                    HashMap<String, Object> metaData = new HashMap<>();

                    for (String meta : ZP400.keySet()) {
                        String url = ADDITONAL_URL + ZP400.get(meta) + String.format(FIND_BY_GUID, formType) + tenderGuid;
                        String additionalMetaData = downloadFromUrl(url).getSourceData();
                        metaData.put(meta, additionalMetaData);
                    }

                    allMetaData.put(tenderGuid, metaData);
                }

                if (formType.equals("ZP403")) {
                    HashMap<String, Object> metaData = new HashMap<>();

                    for (String meta : ZP403.keySet()) {
                        String url = ADDITONAL_URL + ZP403.get(meta) + String.format(FIND_BY_GUID, "") + tenderGuid;
                        String additionalMetaData = downloadFromUrl(url).getSourceData();
                        metaData.put(meta, additionalMetaData);
                    }

                    allMetaData.put(tenderGuid, metaData);
                }

                if (formType.equals("ZP408")) {
                    HashMap<String, Object> metaData = new HashMap<>();

                    for (String meta : ZP408.keySet()) {
                        String url = ADDITONAL_URL + ZP408.get(meta) + String.format(FIND_BY_GUID, "") + tenderGuid;
                        String additionalMetaData = downloadFromUrl(url).getSourceData();
                        metaData.put(meta, additionalMetaData);
                    }

                    allMetaData.put(tenderGuid, metaData);
                }

                if (formType.equals("ZP409")) {
                    HashMap<String, Object> metaData = new HashMap<>();

                    for (String meta : ZP409.keySet()) {
                        String url = ADDITONAL_URL + ZP409.get(meta) + String.format(FIND_BY_GUID, "") + tenderGuid;
                        String additionalMetaData = downloadFromUrl(url).getSourceData();
                        metaData.put(meta, additionalMetaData);
                    }

                    allMetaData.put(tenderGuid, metaData);
                }
            }

            if (!rawTenders.isEmpty()) {
                rawData.setMetaData(allMetaData);
                result.add(rawData);
            }
        } else {
            logger.error("Invalid url, url NULL");
            throw new UnrecoverableException("Invalid url, url NULL");
        }

        // return result
        return result;
    }

    @Override
    public String getVersion() {
        return VERSION;
    }

    @Override
    protected void postProcess(final T raw) {

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
     * A.
     *
     * @param sourceDataUrl a
     * @return a
     */
    private T downloadFromUrl(final String sourceDataUrl) {
        int attempts = 0;
        T rawData = rawDao.getEmptyInstance();

        while (attempts < MAX_ATTEMPTS) {
            try {
                final Connection.Response response = DownloaderUtils.getUrlResponse(sourceDataUrl, DOWNLOAD_TIMEOUT);
                logger.info("Downloaded data from {}", sourceDataUrl);
                rawData.setSourceData(DownloaderUtils.getResponseBody(response));
                rawData.setSourceUrl(new URL(sourceDataUrl));
                rawData.setSourceDataMimeType(response.contentType());
                break;
            } catch (RecoverableException e) {
                attempts++;
                logger.warn("Server halted request, trying again for {} time", attempts);
            } catch (UnrecoverableException e) {
                attempts++;
                logger.warn("Download timed out, trying again for {} time", attempts);
            } catch (MalformedURLException e) {
                logger.error("Download timed out, trying again for {} time", attempts);
                throw new UnrecoverableException("Not url: " + sourceDataUrl);
            }
        }

        if (attempts >= MAX_ATTEMPTS) {
            logger.error("Not able to download from url: {}", sourceDataUrl);
            throw new UnrecoverableException("Not able to download from url: " + sourceDataUrl);
        }

        return rawData;
    }
}
