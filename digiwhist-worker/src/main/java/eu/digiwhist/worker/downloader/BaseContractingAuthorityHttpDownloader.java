package eu.digiwhist.worker.downloader;

import eu.digiwhist.dataaccess.dao.DAOFactory;
import eu.dl.dataaccess.dao.RawDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.raw.downloader.BaseHttpDownloader;

/**
 * Simple HTTP Downloader abstract class for Contracting Authorities.
 */
public abstract class BaseContractingAuthorityHttpDownloader extends BaseHttpDownloader<RawData> {
    @Override
    public final RawDAO getRawDataDao() {
        return DAOFactory.getDAOFactory()
                .getRawContractingAuthorityDAO(getName(), getVersion());
    }

    @Override
    protected final TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }
}
