package eu.digiwhist.dataaccess.dao.mongo;

import eu.dl.dataaccess.dao.mongo.MongoRawDataDAO;

/**
 * Raw Tender DAO implementation for MongoDB.
 */
public class MongoRawTenderDAO extends MongoRawDataDAO {
    private static final String RAW_TENDER_COLLECTION_NAME = "rawTender";

    @Override
    protected final String getCollectionName() {
        return RAW_TENDER_COLLECTION_NAME;
    }
}
