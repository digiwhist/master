package eu.datlab.worker.clean;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.dataaccess.dao.CleanDAO;
import eu.dl.dataaccess.dao.ParsedDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.worker.clean.BaseTenderCleaner;

/**
 * This class covers the main functionality for the cleaners implementation.
 *
 * @author Kuba Krafka
 *
 */
public abstract class BaseDatlabTenderCleaner extends BaseTenderCleaner {

    /**
     * Default constructor.
     */
    protected BaseDatlabTenderCleaner() {
        super();
    }


    @Override
    protected final CleanDAO getCleanDAO() {
        return DAOFactory.getDAOFactory().getCleanTenderDAO(getName(), getVersion());
    }


    @Override
    protected final ParsedDAO getParsedDAO() {
        return DAOFactory.getDAOFactory().getParsedTenderDAO(getName(), getVersion());
    }

    @Override
    protected final TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }
}
