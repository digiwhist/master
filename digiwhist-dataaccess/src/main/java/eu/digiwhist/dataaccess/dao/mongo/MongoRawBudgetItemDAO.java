package eu.digiwhist.dataaccess.dao.mongo;

import eu.digiwhist.dataaccess.dao.RawBudgetItemDAO;
import eu.dl.dataaccess.dao.mongo.MongoRawDataDAO;
import eu.dl.dataaccess.dto.raw.RawData;

/**
 * Raw BudgetItem DAO implementation for MongoDB.
 */
public class MongoRawBudgetItemDAO extends MongoRawDataDAO implements RawBudgetItemDAO<RawData> {
    private static final String RAW_BUDGET_ITEM_COLLECTION_NAME = "rawBudgetItem";

    @Override
    protected final String getCollectionName() {
        return RAW_BUDGET_ITEM_COLLECTION_NAME;
    }
}
