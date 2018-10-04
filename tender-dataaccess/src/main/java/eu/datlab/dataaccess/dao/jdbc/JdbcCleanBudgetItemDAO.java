package eu.datlab.dataaccess.dao.jdbc;

import eu.datlab.dataaccess.dao.CleanBudgetItemDAO;
import eu.datlab.dataaccess.dto.clean.CleanBudgetItem;
import eu.datlab.dataaccess.dto.codetables.BudgetItemReportType;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.jdbc.GenericJdbcDAO;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.List;

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

    @Override
    public final List<CleanBudgetItem> getBodyBudgetItemsInPeriod(final String organizationId, final int yearFrom, final int yearTo) {
        String profitAndLossCodes = "'A.I.1.', 'A.I.2.', 'A.I.3.', 'A.I.4.', 'A.I.5.', 'A.I.6.', 'A.I.7.', 'A.I.8.', 'A.I.9.', 'A.I.10.'," +
            " 'A.I.11.', 'A.I.12.', 'A.I.35.', 'A.I.36.'";

        try {
            PreparedStatement statement = connection.prepareStatement(
                "SELECT * FROM " + getTableWithSchema() +
                " WHERE ((data->>'report' = ? AND data->>'level3Code' IN (" + profitAndLossCodes + "))" +
                        " OR (data->>'report' = ? AND data->>'level3Code' = 'B.I.'))" +
                    " AND data#>>'{body,bodyIds,0,id}' = ? AND (data->>'year')::int BETWEEN ? AND ?");

            statement.setString(1, BudgetItemReportType.PROFIT_AND_LOSS.name());
            statement.setString(2, BudgetItemReportType.CASH_FLOW.name());
            statement.setString(3, organizationId);
            statement.setInt(4, yearFrom - 2);
            statement.setInt(5, yearTo);

            ResultSet rs = statement.executeQuery();

            List<CleanBudgetItem> result = new ArrayList<>();
            while (rs.next()) {
                result.add(createFromResultSet(rs));
            }
            rs.close();
            statement.close();

            return result;
        } catch (Exception e) {
            logger.error("Unable to perform query, because of of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }
}
