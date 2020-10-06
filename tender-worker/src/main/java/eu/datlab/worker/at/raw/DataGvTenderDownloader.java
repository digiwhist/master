package eu.datlab.worker.at.raw;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.core.RecoverableException;
import eu.dl.dataaccess.dao.RawDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.raw.Raw;
import eu.dl.worker.Message;
import eu.dl.worker.raw.downloader.BaseDownloader;
import eu.dl.worker.raw.utils.DownloaderUtils;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * Downloader for data.gv.at tenders.
 *
 * @author Miroslav Brezik
 */
public final class DataGvTenderDownloader extends BaseDownloader {
    private static final String VERSION = "1.0";

    @Override
    protected boolean skipExisting(final Message message) {
        return false;
    }

    @Override
    public List<Raw> downloadAndPopulateRawData(final Message message) {
        Raw raw = rawDao.getEmptyInstance();
        try {
            raw.setSourceUrl(new URL(message.getValue("url")));
        } catch (MalformedURLException e) {
            e.printStackTrace();
        }

        String xml = null;
        try {
             xml = DownloaderUtils.getResponseBody(message.getValue("url"));
        } catch (RecoverableException e) {
            logger.warn("No response from url {}", message.getValue("url"));
            return Collections.emptyList();
        }

        raw.setSourceData(xml.replaceAll("\\u0000", "")
                .replace("\\u0000", "")
                .replace("\n", "")
                .replace("\r", "")
                .replace("\t", "")
        );

        raw.setMetaData(message.getMetaData());

        return Arrays.asList(raw);
    }

    @Override
    public RawDAO getRawDataDao() {
        return DAOFactory.getDAOFactory().getRawBudgetItemDAO(getName(), getVersion());
    }

    @Override
    public String getVersion() {
        return VERSION;
    }

    @Override
    protected void postProcess(final Raw raw) {

    }

    @Override
    protected TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }
}
