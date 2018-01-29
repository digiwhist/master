package eu.digiwhist.dataaccess.dao.mongo;

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
import eu.dl.dataaccess.dao.mongo.MongoCrawlerAuditDAO;
import eu.dl.dataaccess.dao.mongo.MongoTransactionUtils;

/**
 * DAO factory implementation for MongoDB database.
 */
public class MongoDAOFactory extends DAOFactory {

    @Override
    public final CrawlerAuditDAO getCrawlerAuditDAO(final String workerName, final String workerVersion) {
        return (CrawlerAuditDAO) new MongoCrawlerAuditDAO().populateWithWorkerMetadata(workerName, workerVersion);
    }

    // TENDER DAOs

    @Override
    public final RawDataDAO getRawTenderDAO(final String workerName, final String workerVersion) {
        return (RawDataDAO) new MongoRawTenderDAO().populateWithWorkerMetadata(workerName, workerVersion);
    }

    @Override
    public final ParsedTenderDAO getParsedTenderDAO(final String workerName, final String workerVersion) {
        return (ParsedTenderDAO) new MongoParsedTenderDAO().populateWithWorkerMetadata(workerName, workerVersion);
    }

    @Override
    public final CleanTenderDAO getCleanTenderDAO(final String workerName, final String workerVersion) {
        return (CleanTenderDAO) new MongoCleanTenderDAO().populateWithWorkerMetadata(workerName, workerVersion);
    }

    // CONTRACTING AUTHORITY DAOs

    @Override
    public final RawContractingAuthorityDAO getRawContractingAuthorityDAO(final String workerName,
            final String workerVersion) {
        return (RawContractingAuthorityDAO) new MongoRawContractingAuthorityDAO().populateWithWorkerMetadata(workerName,
                workerVersion);
    }

    @Override
    public final ParsedContractingAuthorityDAO getParsedContractingAuthorityDAO(final String workerName,
            final String workerVersion) {
        return (ParsedContractingAuthorityDAO) new MongoParsedContractingAuthorityDAO().populateWithWorkerMetadata(
                workerName, workerVersion);
    }

    // PUBLIC OFFICIAL DAOs

    @Override
    public final RawPublicOfficialDAO getRawPublicOfficialDAO(final String workerName, final String workerVersion) {
        return (RawPublicOfficialDAO) new MongoRawPublicOfficialDAO().populateWithWorkerMetadata(workerName,
                workerVersion);
    }

    @Override
    public final ParsedPublicOfficialDAO getParsedPublicOfficialDAO(final String workerName,
            final String workerVersion) {
        return (ParsedPublicOfficialDAO) new MongoParsedPublicOfficialDAO().populateWithWorkerMetadata(workerName,
                workerVersion);
    }

    // ASSET DECLARATION DAOs

    @Override
    public final RawAssetDeclarationDAO getRawAssetDeclarationDAO(final String workerName, final String workerVersion) {
        return (RawAssetDeclarationDAO) new MongoRawAssetDeclarationDAO().populateWithWorkerMetadata(workerName,
                workerVersion);
    }

    // MATCHED DAOS

    @Override
    public final MatchedBodyDAO getMatchedBodyDAO(final String workerName, final String workerVersion,
            final List<Pair<String, String>> additionalMatchers) {
        return (MatchedBodyDAO) new MongoMatchedBodyDAO().populateWithWorkerMetadata(workerName, workerVersion)
                .setAdditionalWorkers(additionalMatchers);
    }

    @Override
    public final MatchedTenderDAO getMatchedTenderDAO(final String workerName, final String workerVersion,
            final List<Pair<String, String>> additionalMatchers) {
        return (MatchedTenderDAO) new MongoMatchedTenderDAO().populateWithWorkerMetadata(workerName, workerVersion)
                .setAdditionalWorkers(additionalMatchers);
    }

    @Override
    public final ManualMatchDAO getManualMatchDAO(final String workerName, final String workerVersion) {
        return (ManualMatchDAO) new MongoManualMatchDAO().populateWithWorkerMetadata(workerName, workerVersion);
    }

    // MASTERED DAOS

    @Override
    public final MasterBodyDAO getMasterBodyDAO(final String workerName, final String workerVersion) {
        return (MasterBodyDAO) new MongoMasterBodyDAO().populateWithWorkerMetadata(workerName, workerVersion);
    }

    @Override
    public final MasterTenderDAO getMasterTenderDAO(final String workerName, final String workerVersion) {
        return (MasterTenderDAO) new MongoMasterTenderDAO().populateWithWorkerMetadata(workerName, workerVersion);
    }

    //BUDGET ITEM DAOs

    @Override
    public final RawDataDAO getRawBudgetItemDAO(final String workerName, final String workerVersion) {
        return (RawDataDAO) new MongoRawBudgetItemDAO().populateWithWorkerMetadata(workerName, workerVersion);
    }

    @Override
    public final ParsedBudgetItemDAO getParsedBudgetItemDAO(final String workerName, final String workerVersion) {
        return (ParsedBudgetItemDAO) new MongoParsedBudgetItemDAO().populateWithWorkerMetadata(workerName,
                workerVersion);
    }

    @Override
    public final CleanBudgetItemDAO getCleanBudgetItemDAO(final String workerName, final String workerVersion) {
        return (CleanBudgetItemDAO) new MongoCleanBudgetItemDAO().populateWithWorkerMetadata(workerName, workerVersion);
    }

    @Override
    public final TransactionUtils getTransactionUtils() {
        return MongoTransactionUtils.getInstance();
    }

    @Override
    public final EtalonBodyDAO getBVDEtalonBodyDAO() {
        throw new UnsupportedOperationException("Operation not supported");
    }
}
