package eu.digiwhist.dataaccess.dao.hibernate;

import java.util.List;

import org.apache.commons.lang3.tuple.Pair;

import eu.digiwhist.dataaccess.dao.CleanBudgetItemDAO;
import eu.digiwhist.dataaccess.dao.DAOFactory;
import eu.digiwhist.dataaccess.dao.ParsedBudgetItemDAO;
import eu.digiwhist.dataaccess.dao.ParsedContractingAuthorityDAO;
import eu.digiwhist.dataaccess.dao.ParsedPublicOfficialDAO;
import eu.digiwhist.dataaccess.dao.RawAssetDeclarationDAO;
import eu.digiwhist.dataaccess.dao.RawContractingAuthorityDAO;
import eu.digiwhist.dataaccess.dao.RawPublicOfficialDAO;
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
import eu.dl.dataaccess.dao.hibernate.HibernateCrawlerAuditDAO;
import eu.dl.dataaccess.dao.hibernate.HibernateRawDataDAO;
import eu.dl.dataaccess.dao.hibernate.HibernateTransactionUtils;
import eu.dl.dataaccess.dto.matched.ManualMatch;

/**
 * DAO factory implementation for Hibernate data sources.
 */
public final class HibernateDAOFactory extends DAOFactory {

    @Override
    public CrawlerAuditDAO getCrawlerAuditDAO(final String workerName, final String workerVersion) {
        return (CrawlerAuditDAO) new HibernateCrawlerAuditDAO().populateWithWorkerMetadata(workerName, workerVersion);
    }

    @Override
    public RawDataDAO getRawTenderDAO(final String workerName, final String workerVersion) {
        return (RawDataDAO) new HibernateRawDataDAO().populateWithWorkerMetadata(workerName, workerVersion);
    }

    @Override
    public ParsedTenderDAO getParsedTenderDAO(final String workerName, final String workerVersion) {
        return (ParsedTenderDAO) new HibernateParsedTenderDAO().populateWithWorkerMetadata(workerName, workerVersion);
    }

    @Override
    public CleanTenderDAO getCleanTenderDAO(final String workerName, final String workerVersion) {
        return (CleanTenderDAO) new HibernateCleanTenderDAO().populateWithWorkerMetadata(workerName, workerVersion);
    }

    @Override
    public RawContractingAuthorityDAO getRawContractingAuthorityDAO(final String workerName,
            final String workerVersion) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public ParsedContractingAuthorityDAO getParsedContractingAuthorityDAO(final String workerName,
            final String workerVersion) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public RawPublicOfficialDAO getRawPublicOfficialDAO(final String workerName, final String workerVersion) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public ParsedPublicOfficialDAO getParsedPublicOfficialDAO(final String workerName, final String workerVersion) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public RawAssetDeclarationDAO getRawAssetDeclarationDAO(final String workerName, final String workerVersion) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public MatchedBodyDAO getMatchedBodyDAO(final String workerName, final String workerVersion,
            final List<Pair<String, String>> additionalMatchers) {
        return (MatchedBodyDAO) new HibernateMatchedBodyDAO().populateWithWorkerMetadata(workerName, workerVersion)
                .setAdditionalWorkers(additionalMatchers);
    }

    @Override
    public MatchedTenderDAO getMatchedTenderDAO(final String workerName, final String workerVersion,
            final List<Pair<String, String>> additionalMatchers) {
        return (MatchedTenderDAO) new HibernateMatchedTenderDAO().populateWithWorkerMetadata(workerName, workerVersion)
                .setAdditionalWorkers(additionalMatchers);
    }

    @Override
    public ManualMatchDAO getManualMatchDAO(final String workerName, final String workerVersion) {
        return (ManualMatchDAO<ManualMatch>) new HibernateManualMatchDAO<ManualMatch>().populateWithWorkerMetadata(
                workerName, workerVersion);
    }

    @Override
    public MasterBodyDAO getMasterBodyDAO(final String workerName, final String workerVersion) {
        // TODO Auto-generated method stub
        return (MasterBodyDAO) new HibernateMasterBodyDAO().populateWithWorkerMetadata(workerName, workerVersion);
    }

    @Override
    public MasterTenderDAO getMasterTenderDAO(final String workerName, final String workerVersion) {
        // TODO Auto-generated method stub
        return (MasterTenderDAO) new HibernateMasterTenderDAO().populateWithWorkerMetadata(workerName, workerVersion);
    }

    @Override
    public RawDataDAO getRawBudgetItemDAO(final String workerName, final String workerVersion) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public ParsedBudgetItemDAO getParsedBudgetItemDAO(final String workerName, final String workerVersion) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public CleanBudgetItemDAO getCleanBudgetItemDAO(final String workerName, final String workerVersion) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public TransactionUtils getTransactionUtils() {
        return HibernateTransactionUtils.getInstance();
    }

    @Override
    public EtalonBodyDAO getBVDEtalonBodyDAO() {
        return HibernateBVDEtalonBodyDAO.getInstance();
    }
}
