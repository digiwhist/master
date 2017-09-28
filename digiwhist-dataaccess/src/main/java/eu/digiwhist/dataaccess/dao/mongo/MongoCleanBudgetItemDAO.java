package eu.digiwhist.dataaccess.dao.mongo;

import eu.digiwhist.dataaccess.dto.clean.CleanBudgetItem;
import eu.dl.dataaccess.dao.CleanDAO;
import eu.dl.dataaccess.dao.mongo.GenericMongoDAO;

/**
 * Clean BudgetItem DAO implementation for MongoDB.
 */
public class MongoCleanBudgetItemDAO extends GenericMongoDAO<CleanBudgetItem>
    implements CleanDAO<CleanBudgetItem> {

    private static final String CLEAN_BUDGET_ITEM_COLLECTION_NAME = "cleanBudgetItem";

    @Override
    protected final Class<CleanBudgetItem> getDTOClass() {
        return CleanBudgetItem.class;
    }

    @Override
    protected final String getCollectionName() {
        return CLEAN_BUDGET_ITEM_COLLECTION_NAME;
    }

    @Override
    public final CleanBudgetItem getEmptyInstance() {
        return new CleanBudgetItem();
    }
}
