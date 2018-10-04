package eu.datlab.worker.matched;

import eu.datlab.dataaccess.dao.DAOFactory;
import eu.dl.dataaccess.dao.CleanTenderDAO;
import eu.dl.dataaccess.dao.EtalonBodyDAO;
import eu.dl.dataaccess.dao.ManualMatchDAO;
import eu.dl.dataaccess.dao.MatchedBodyDAO;
import eu.dl.dataaccess.dao.MatchedTenderDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import eu.dl.worker.matched.BaseTenderMatcher;
import org.apache.commons.lang3.tuple.Pair;

import java.util.List;

/**
 * Base tender matcher providing correct DAO implementations.
 */
public abstract class BaseDatlabTenderMatcher extends BaseTenderMatcher {

    /**
     * Constructor that takes list of additional matchers.
     *
     * @param additionalMatchers
     *         list of matchers (worker names and versions) whose data should be included in searching for a match
     */
    protected BaseDatlabTenderMatcher(final List<Pair<String, String>> additionalMatchers) {
        super(additionalMatchers);
    }

    /**
     * Default constructor.
     */
    protected BaseDatlabTenderMatcher() {
        super();
    }

    @Override
    protected final CleanTenderDAO getCleanTenderDAO() {
        return DAOFactory.getDAOFactory().getCleanTenderDAO(getName(), getVersion());
    }

    @Override
    protected final MatchedTenderDAO getMatchedTenderDAO(final List<Pair<String, String>> additionalMatchers) {
        return DAOFactory.getDAOFactory().getMatchedTenderDAO(getName(), getVersion(), additionalMatchers);
    }

    @Override
    protected final MatchedBodyDAO getMatchedBodyDAO(final List<Pair<String, String>> additionalMatchers) {
        return DAOFactory.getDAOFactory().getMatchedBodyDAO(getName(), getVersion(), additionalMatchers);
    }

    @Override
    protected final ManualMatchDAO getManualMatchDAO() {
        return DAOFactory.getDAOFactory().getManualMatchDAO(getName(), getVersion());
    }

    @Override
    protected final TransactionUtils getTransactionUtils() {
        return DAOFactory.getDAOFactory().getTransactionUtils();
    }

    @Override
    protected final EtalonBodyDAO getEtalonBodyDAO() {
        return DAOFactory.getDAOFactory().getBVDEtalonBodyDAO();
    }
}
