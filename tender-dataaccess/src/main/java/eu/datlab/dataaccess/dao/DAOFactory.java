package eu.datlab.dataaccess.dao;

import eu.datlab.dataaccess.dao.jdbc.JdbcDAOFactory;
import eu.dl.core.config.Config;
import eu.dl.dataaccess.dao.CleanTenderDAO;
import eu.dl.dataaccess.dao.CrawlerAuditDAO;
import eu.dl.dataaccess.dao.EtalonBodyDAO;
import eu.dl.dataaccess.dao.ManualMatchDAO;
import eu.dl.dataaccess.dao.MasterBodyDAO;
import eu.dl.dataaccess.dao.MasterTenderDAO;
import eu.dl.dataaccess.dao.MatchedBodyDAO;
import eu.dl.dataaccess.dao.MatchedTenderDAO;
import eu.dl.dataaccess.dao.ParsedTenderDAO;
import eu.dl.dataaccess.dao.RawDataDAO;
import eu.dl.dataaccess.dao.TransactionUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

/**
 * Abstract DAO factory class for creating DAO objects.
 */
public abstract class DAOFactory {

    /**
     * Default factory type.
     */
    private static final DAOFactoryType DEFAULT_FACTORY_TYPE = DAOFactoryType.JDBC;

    /**
     * Logger.
     */
    private static final Logger logger = LoggerFactory.getLogger(DAOFactory.class);

    /**
     * DAO types supported by the factory.
     */
    public enum DAOFactoryType {
        /**
         * Values.
         */
        JDBC
    }

    /**
     * Creates and returns new default DAO factory.
     *
     * @return default DAO factory
     */
    public static DAOFactory getDefaultDAOFactory() {
        return getDAOFactory(DEFAULT_FACTORY_TYPE);
    }

    /**
     * Creates and returns new DAO factory of specified type.
     *
     * @param factoryType
     *         type of requested DAO factory
     *
     * @return new DAO factory of specified type
     */
    public static DAOFactory getDAOFactory(final DAOFactoryType factoryType) {
        switch (factoryType) {
            case JDBC:
                return new JdbcDAOFactory();
            default:
                logger.error("Unknown factory type while trying to create DAO factory.");
                throw new IllegalArgumentException("Unknown factory type.");
        }
    }

    /**
     * Creates and returns new DAO factory of type specified in configuration.
     *
     * @return new DAO factory of configured type
     */
    public static DAOFactory getDAOFactory() {
        return getDAOFactory(DAOFactoryType.valueOf(Config.getInstance().getParam("daofactory.type")));
    }

    // There will be a method for each DAO that can be created. The concrete factories will have to implement these
    // methods.

    /**
     * Gets the Crawler Audit DAO.
     *
     * @param workerName
     *         name of the worker manipulating with data via this DAO
     * @param workerVersion
     *         version of the worker manipulating with data via this DAO
     *
     * @return DAO object for managing crawler audit log.
     */
    public abstract CrawlerAuditDAO getCrawlerAuditDAO(String workerName, String workerVersion);

    /**
     * Gets the Raw Tender DAO.
     *
     * @param workerName
     *         name of the worker manipulating with data via this DAO
     * @param workerVersion
     *         version of the worker manipulating with data via this DAO
     *
     * @return DAO object for managing raw tender data
     */
    public abstract RawDataDAO getRawTenderDAO(String workerName, String workerVersion);

    /**
     * Gets the Parsed Tender DAO.
     *
     * @param workerName
     *         name of the worker manipulating with data via this DAO
     * @param workerVersion
     *         version of the worker manipulating with data via this DAO
     *
     * @return DAO object for managing parsed tenders
     */
    public abstract ParsedTenderDAO getParsedTenderDAO(String workerName, String workerVersion);

    /**
     * Gets the Clean Tender DAO.
     *
     * @param workerName
     *         name of the worker manipulating with data via this DAO
     * @param workerVersion
     *         version of the worker manipulating with data via this DAO
     *
     * @return DAO object for managing clean tenders
     */
    public abstract CleanTenderDAO getCleanTenderDAO(String workerName, String workerVersion);

    /**
     * Gets the Raw Contracting Authority DAO.
     *
     * @param workerName
     *         name of the worker manipulating with data via this DAO
     * @param workerVersion
     *         version of the worker manipulating with data via this DAO
     *
     * @return DAO object for managing raw contracting authority data
     */
    public abstract RawDataDAO getRawContractingAuthorityDAO(String workerName, String workerVersion);

    /**
     * Gets the Parsed Contracting Authority DAO.
     *
     * @param workerName
     *         name of the worker manipulating with data via this DAO
     * @param workerVersion
     *         version of the worker manipulating with data via this DAO
     *
     * @return DAO object for managing parsed contracting authorities
     */
    public abstract ParsedContractingAuthorityDAO getParsedContractingAuthorityDAO(String workerName,
            String workerVersion);

    /**
     * Gets the Raw Public Official DAO.
     *
     * @param workerName
     *         name of the worker manipulating with data via this DAO
     * @param workerVersion
     *         version of the worker manipulating with data via this DAO
     *
     * @return DAO object for managing raw public official data
     */
    public abstract RawDataDAO getRawPublicOfficialDAO(String workerName, String workerVersion);

    /**
     * Gets the Parsed Public Official DAO.
     *
     * @param workerName
     *         name of the worker manipulating with data via this DAO
     * @param workerVersion
     *         version of the worker manipulating with data via this DAO
     *
     * @return DAO object for managing parsed public officials
     */
    public abstract ParsedPublicOfficialDAO getParsedPublicOfficialDAO(String workerName, String workerVersion);

    /**
     * Gets the Raw Asset Declaration DAO.
     *
     * @param workerName
     *         name of the worker manipulating with data via this DAO
     * @param workerVersion
     *         version of the worker manipulating with data via this DAO
     *
     * @return DAO object for managing raw asset declaration data
     */
    public abstract RawAssetDeclarationDAO getRawAssetDeclarationDAO(String workerName, String workerVersion);

    /**
     * Gets the matched body DAO.
     *
     * @param workerName
     *         name of the worker manipulating with data via this DAO
     * @param workerVersion
     *         version of the worker manipulating with data via this DAO
     * @param additionalMatchers
     *         list of matchers (worker names and versions) whose data should be included in searching for a match (for
     *         cross-source matching purposes)
     *
     * @return DAO object for managing matched bodies
     */
    public abstract MatchedBodyDAO getMatchedBodyDAO(String workerName, String workerVersion,
            List<Pair<String, String>> additionalMatchers);

    /**
     * Gets the matched tender DAO.
     *
     * @param workerName
     *         name of the worker manipulating with data via this DAO
     * @param workerVersion
     *         version of the worker manipulating with data via this DAO
     * @param additionalMatchers
     *         list of matchers (worker names and versions) whose data should be included in searching for a match (for
     *         cross-source matching purposes)
     *
     * @return DAO object for managing matched tenders
     */
    public abstract MatchedTenderDAO getMatchedTenderDAO(String workerName, String workerVersion,
            List<Pair<String, String>> additionalMatchers);

    /**
     * Gets the manual match DAO.
     *
     * @param workerName
     *         name of the worker manipulating with data via this DAO
     * @param workerVersion
     *         version of the worker manipulating with data via this DAO
     *
     * @return DAO object for managing manual matches
     */
    public abstract ManualMatchDAO getManualMatchDAO(String workerName, String workerVersion);

    /**
     * Gets the mastered body DAO.
     *
     * @param workerName
     *         name of the worker manipulating with data via this DAO
     * @param workerVersion
     *         version of the worker manipulating with data via this DAO
     *
     * @return DAO object for managing mastered bodies
     */
    public abstract MasterBodyDAO getMasterBodyDAO(String workerName, String workerVersion);

    /**
     * Gets the mastered tender DAO.
     *
     * @param workerName
     *         name of the worker manipulating with data via this DAO
     * @param workerVersion
     *         version of the worker manipulating with data via this DAO
     *
     * @return DAO object for managing mastered tenders
     */
    public abstract MasterTenderDAO getMasterTenderDAO(String workerName, String workerVersion);

    /**
     * Gets the Raw BudgetItem DAO.
     *
     * @param workerName
     *         name of the worker manipulating with data via this DAO
     * @param workerVersion
     *         version of the worker manipulating with data via this DAO
     *
     * @return DAO object for managing raw budget item data
     */
    public abstract RawDataDAO getRawBudgetItemDAO(String workerName, String workerVersion);

    /**
     * Gets the Parsed BudgetItem DAO.
     *
     * @param workerName
     *         name of the worker manipulating with data via this DAO
     * @param workerVersion
     *         version of the worker manipulating with data via this DAO
     *
     * @return DAO object for managing parsed budget item
     */
    public abstract ParsedBudgetItemDAO getParsedBudgetItemDAO(String workerName, String workerVersion);

    /**
     * Gets the Clean BudgetItem DAO.
     *
     * @param workerName
     *         name of the worker manipulating with data via this DAO
     * @param workerVersion
     *         version of the worker manipulating with data via this DAO
     *
     * @return DAO object for managing parsed budget item
     */
    public abstract CleanBudgetItemDAO getCleanBudgetItemDAO(String workerName, String workerVersion);

    /**
     * Gets transaction utils used to handle transactions.
     *
     * @return transaction utils
     */
    public abstract TransactionUtils getTransactionUtils();

    /**
     * Gets BvD etalon body DAO.
     *
     * @return DAO obejct for managing BvD etalon body
     */
    public abstract EtalonBodyDAO getBVDEtalonBodyDAO();

    /**
     * Gets zIndex indicator DAO.
     *
     * @return DAO obejct for managing zIndex indicator
     */
    public abstract ZIndexIndicatorDAO getZIndexIndicatorDAO();
}
