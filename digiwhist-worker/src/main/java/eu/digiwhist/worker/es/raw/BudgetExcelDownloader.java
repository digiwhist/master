package eu.digiwhist.worker.es.raw;

import eu.digiwhist.dataaccess.dao.DAOFactory;
import eu.dl.dataaccess.dao.RawDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.worker.raw.downloader.BaseHttpDownloader;

/**
 * Spanish excel documents downloader.
 */
public class BudgetExcelDownloader extends BaseHttpDownloader {
    @Override
    public final RawDAO getRawDataDao() {
        return DAOFactory.getDAOFactory().getRawBudgetItemDAO(getName(), getVersion());
    }

    @Override
    protected final TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }
}
