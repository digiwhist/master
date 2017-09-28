package eu.digiwhist.dataaccess.dao.mongo;

import eu.digiwhist.dataaccess.dao.RawContractingAuthorityDAO;
import eu.dl.dataaccess.dao.mongo.MongoRawDataDAO;
import eu.dl.dataaccess.dto.raw.RawData;

/**
 * Raw Contracting Authority DAO implementation for MongoDB.
 */
public class MongoRawContractingAuthorityDAO extends MongoRawDataDAO
        implements RawContractingAuthorityDAO<RawData> {
    private static final String RAW_CONTRACTING_AUTHORITY_COLLECTION_NAME = "rawContractingAuthority";

    @Override
    protected final String getCollectionName() {
        return RAW_CONTRACTING_AUTHORITY_COLLECTION_NAME;
    }
}
