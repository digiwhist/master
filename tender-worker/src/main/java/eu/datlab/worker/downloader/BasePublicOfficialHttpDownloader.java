package eu.datlab.worker.downloader;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.dataaccess.dao.RawDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.raw.downloader.BaseHttpDownloader;

/**
 * Simple HTTP Downloader abstract class for Public Officials.
 */
public abstract class BasePublicOfficialHttpDownloader extends BaseHttpDownloader<RawData> {

    @Override
    public final RawDAO getRawDataDao() {
        return DAOFactory.getDAOFactory().getRawPublicOfficialDAO(getName(), getVersion());
    }

    @Override
    protected final TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }
}

