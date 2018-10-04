package eu.datlab.worker.master;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.dataaccess.dao.MasterDAO;
import eu.dl.dataaccess.dao.MatchedDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.dataaccess.dto.master.MasterBody;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.worker.master.BaseBodyMaster;

/**
 * Base class for Datlab body masters.
 */
public abstract class BaseDatlabBodyMaster extends BaseBodyMaster<MatchedBody, MasterBody> {
    @Override
    protected final MatchedDAO getMatchedDAO() {
        return DAOFactory.getDAOFactory().getMatchedBodyDAO(getName(), getVersion(), null);
    }

    @Override
    protected final MasterDAO getMasterDAO() {
        return DAOFactory.getDAOFactory().getMasterBodyDAO(getName(), getVersion());
    }

    @Override
    protected final void registerIndicatorPlugins() {

    }

    @Override
    protected final TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }

    @Override
    protected void registerProjectSpecificPlugins() {

    }
}
