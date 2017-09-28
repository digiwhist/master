package eu.digiwhist.dataaccess.dao.mongo;

import eu.digiwhist.dataaccess.dao.ParsedBudgetItemDAO;
import eu.digiwhist.dataaccess.dto.parsed.ParsedBudgetItem;
import eu.dl.dataaccess.dao.mongo.GenericMongoDAO;

/**
 * Parsed BudgetItem DAO implementation for MongoDB.
 */
public class MongoParsedBudgetItemDAO extends GenericMongoDAO<ParsedBudgetItem> implements
    ParsedBudgetItemDAO<ParsedBudgetItem> {

    private static final String PARSED_BUDGET_ITEM_COLLECTION_NAME = "parsedBudgetItem";

    @Override
    protected final Class<ParsedBudgetItem> getDTOClass() {
        return ParsedBudgetItem.class;
    }

    @Override
    protected final String getCollectionName() {
        return PARSED_BUDGET_ITEM_COLLECTION_NAME;
    }

    @Override
    public final ParsedBudgetItem getEmptyInstance() {
        return new ParsedBudgetItem();
    }
}
