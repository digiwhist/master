package eu.datlab.dataaccess.dao.jdbc;

import java.util.List;

import eu.dl.dataaccess.dao.jdbc.JdbcMatchedBodyDAO;
import org.apache.commons.lang3.tuple.Pair;

import eu.datlab.dataaccess.dao.CleanBudgetItemDAO;
import eu.datlab.dataaccess.dao.DAOFactory;
import eu.datlab.dataaccess.dao.ParsedBudgetItemDAO;
import eu.datlab.dataaccess.dao.ParsedContractingAuthorityDAO;
import eu.datlab.dataaccess.dao.ParsedPublicOfficialDAO;
import eu.datlab.dataaccess.dao.RawAssetDeclarationDAO;
import eu.datlab.dataaccess.dao.ZIndexIndicatorDAO;
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
import eu.dl.dataaccess.dao.jdbc.JdbcCrawlerAuditDAO;
import eu.dl.dataaccess.dao.jdbc.JdbcRawDataDAO;
import eu.dl.dataaccess.dao.jdbc.JdbcTransactionUtils;
import eu.dl.dataaccess.dto.parsed.ParsedPublicOfficial;

/**
 * DAO factory implementation for JDBC data sources.
 */
public final class JdbcDAOFactory extends DAOFactory {

    @Override
    public CrawlerAuditDAO getCrawlerAuditDAO(final String workerName, final String workerVersion) {
        return (CrawlerAuditDAO) new JdbcCrawlerAuditDAO().populateWithWorkerMetadata(workerName, workerVersion);
    }

    @Override
    public RawDataDAO getRawTenderDAO(final String workerName, final String workerVersion) {
        return (RawDataDAO) new JdbcRawDataDAO().populateWithWorkerMetadata(workerName, workerVersion);
    }

    @Override
    public ParsedTenderDAO getParsedTenderDAO(final String workerName, final String workerVersion) {
        return (ParsedTenderDAO) new JdbcParsedTenderDAO().populateWithWorkerMetadata(workerName, workerVersion);
    }

    @Override
    public CleanTenderDAO getCleanTenderDAO(final String workerName, final String workerVersion) {
        return (CleanTenderDAO) new JdbcCleanTenderDAO().populateWithWorkerMetadata(workerName, workerVersion);
    }

    @Override
    public RawDataDAO getRawContractingAuthorityDAO(final String workerName, final String workerVersion) {
        return (RawDataDAO) new JdbcRawDataDAO().populateWithWorkerMetadata(workerName, workerVersion);
    }

    @Override
    public ParsedContractingAuthorityDAO getParsedContractingAuthorityDAO(final String workerName,
            final String workerVersion) {
        return (ParsedContractingAuthorityDAO) new JdbcParsedContractingAuthorityDAO().populateWithWorkerMetadata(
                workerName, workerVersion);
    }

    @Override
    public RawDataDAO getRawPublicOfficialDAO(final String workerName, final String workerVersion) {
        return (RawDataDAO) new JdbcRawDataDAO().populateWithWorkerMetadata(workerName, workerVersion);
    }

    @Override
    public ParsedPublicOfficialDAO getParsedPublicOfficialDAO(final String workerName, final String workerVersion) {
        return (ParsedPublicOfficialDAO<ParsedPublicOfficial>) new JdbcParsedPublicOfficialDAO()
                .populateWithWorkerMetadata(
                workerName, workerVersion);
    }

    @Override
    public RawAssetDeclarationDAO getRawAssetDeclarationDAO(final String workerName, final String workerVersion) {
        return (RawAssetDeclarationDAO) new JdbcRawDataDAO().populateWithWorkerMetadata(workerName, workerVersion);
    }

    @Override
    public MatchedBodyDAO getMatchedBodyDAO(final String workerName, final String workerVersion,
            final List<Pair<String, String>> additionalMatchers) {
        return (MatchedBodyDAO) new JdbcMatchedBodyDAO().populateWithWorkerMetadata(workerName, workerVersion)
                .setAdditionalWorkers(additionalMatchers);
    }

    @Override
    public MatchedTenderDAO getMatchedTenderDAO(final String workerName, final String workerVersion,
            final List<Pair<String, String>> additionalMatchers) {
        return (MatchedTenderDAO) new JdbcMatchedTenderDAO().populateWithWorkerMetadata(workerName, workerVersion)
                .setAdditionalWorkers(additionalMatchers);
    }

    @Override
    public ManualMatchDAO getManualMatchDAO(final String workerName, final String workerVersion) {
        return (ManualMatchDAO) new JdbcManualMatchDAO().populateWithWorkerMetadata(workerName, workerVersion);
    }

    @Override
    public MasterBodyDAO getMasterBodyDAO(final String workerName, final String workerVersion) {
        return (MasterBodyDAO) new JdbcMasterBodyDAO().populateWithWorkerMetadata(workerName, workerVersion);
    }

    @Override
    public MasterTenderDAO getMasterTenderDAO(final String workerName, final String workerVersion) {
        return (MasterTenderDAO) new JdbcMasterTenderDAO().populateWithWorkerMetadata(workerName, workerVersion);
    }

    @Override
    public RawDataDAO getRawBudgetItemDAO(final String workerName, final String workerVersion) {
        return (RawDataDAO) new JdbcRawDataDAO().populateWithWorkerMetadata(workerName, workerVersion);
    }

    @Override
    public ParsedBudgetItemDAO getParsedBudgetItemDAO(final String workerName, final String workerVersion) {
        return (ParsedBudgetItemDAO) new JdbcParsedBudgetItemDAO().populateWithWorkerMetadata(workerName,
                workerVersion);
    }

    @Override
    public CleanBudgetItemDAO getCleanBudgetItemDAO(final String workerName, final String workerVersion) {
        return (CleanBudgetItemDAO) new JdbcCleanBudgetItemDAO().populateWithWorkerMetadata(workerName, workerVersion);
    }

    @Override
    public TransactionUtils getTransactionUtils() {
        return JdbcTransactionUtils.getInstance();
    }

    @Override
    public EtalonBodyDAO getBVDEtalonBodyDAO() {
        return new JdbcBVDEtalonBodyDAO();
    }

    @Override
    public ZIndexIndicatorDAO getZIndexIndicatorDAO() {
        return new JdbcZIndexIndicatorDAO();
    }
}
