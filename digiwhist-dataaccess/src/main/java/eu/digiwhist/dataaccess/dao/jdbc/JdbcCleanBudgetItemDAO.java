package eu.digiwhist.dataaccess.dao.jdbc;

import eu.digiwhist.dataaccess.dao.CleanBudgetItemDAO;
import eu.digiwhist.dataaccess.dto.clean.CleanBudgetItem;
import eu.dl.dataaccess.dao.jdbc.GenericJdbcDAO;

/**
 * JDBC DAO implementation for budget item.
 */
public class JdbcCleanBudgetItemDAO extends GenericJdbcDAO<CleanBudgetItem>
        implements CleanBudgetItemDAO<CleanBudgetItem> {
    private static final String TABLE_NAME = "clean_budget_item";

    @Override
    protected final String getTableWithSchema() {
        return schema + "." + TABLE_NAME;
    }

    @Override
    public final CleanBudgetItem getEmptyInstance() {
        return new CleanBudgetItem();
    }
}
