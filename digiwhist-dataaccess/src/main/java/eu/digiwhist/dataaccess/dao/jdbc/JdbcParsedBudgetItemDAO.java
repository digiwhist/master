package eu.digiwhist.dataaccess.dao.jdbc;

import eu.digiwhist.dataaccess.dao.ParsedBudgetItemDAO;
import eu.digiwhist.dataaccess.dto.parsed.ParsedBudgetItem;
import eu.dl.dataaccess.dao.jdbc.GenericJdbcDAO;

/**
 * JDBC DAO implementation for budget item.
 */
public class JdbcParsedBudgetItemDAO extends GenericJdbcDAO<ParsedBudgetItem>
        implements ParsedBudgetItemDAO<ParsedBudgetItem> {
    private static final String TABLE_NAME = "parsed_budget_item";

    @Override
    protected final String getTableWithSchema() {
        return schema + "." + TABLE_NAME;
    }

    @Override
    public final ParsedBudgetItem getEmptyInstance() {
        return new ParsedBudgetItem();
    }
}
