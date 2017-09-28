package eu.digiwhist.dataaccess.dao.mongo;

import eu.digiwhist.dataaccess.dao.RawPublicOfficialDAO;
import eu.dl.dataaccess.dao.mongo.MongoRawDataDAO;
import eu.dl.dataaccess.dto.raw.RawData;

/**
 * Raw Public official DAO implementation for MongoDB.
 */
public class MongoRawPublicOfficialDAO extends MongoRawDataDAO implements RawPublicOfficialDAO<RawData> {
    private static final String RAW_PUBLIC_OFFICIAL_COLLECTION_NAME = "rawPublicOfficial";

    @Override
    protected final String getCollectionName() {
        return RAW_PUBLIC_OFFICIAL_COLLECTION_NAME;
    }
}
