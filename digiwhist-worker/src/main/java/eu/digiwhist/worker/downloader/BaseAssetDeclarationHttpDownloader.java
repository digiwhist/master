package eu.digiwhist.worker.downloader;

import eu.digiwhist.dataaccess.dao.DAOFactory;
import eu.digiwhist.dataaccess.dao.RawAssetDeclarationDAO;
import eu.dl.dataaccess.dao.RawDataDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.raw.downloader.BaseHttpDownloader;

/**
 * Simple HTTP Downloader abstract class for Tenders.
 */
public abstract class BaseAssetDeclarationHttpDownloader extends BaseHttpDownloader<RawData> {
    private final RawAssetDeclarationDAO dao = DAOFactory.getDAOFactory()
            .getRawAssetDeclarationDAO(getName(), getVersion());

    @Override
    public final RawDataDAO<RawData> getRawDataDao() {
        return dao;
    }

    @Override
    protected final TransactionUtils getTransactionUtils() {
        return DAOFactory.getDefaultDAOFactory().getTransactionUtils();
    }
}
