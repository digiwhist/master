package eu.digiwhist.dataaccess.dao.mongo;

import eu.digiwhist.dataaccess.dao.RawAssetDeclarationDAO;
import eu.dl.dataaccess.dao.mongo.MongoRawDataDAO;
import eu.dl.dataaccess.dto.raw.RawData;

/**
 * Raw Asset Declaration DAO implementation for MongoDB.
 */
public class MongoRawAssetDeclarationDAO extends MongoRawDataDAO implements RawAssetDeclarationDAO<RawData> {
    private static final String RAW_ASSET_DECLARATION_COLLECTION_NAME = "rawAssetDeclaration";

    @Override
    protected final String getCollectionName() {
        return RAW_ASSET_DECLARATION_COLLECTION_NAME;
    }
}
