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
import java.util.Map;
import java.util.stream.Collectors;

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
    public final List<CleanBudgetItem> getBodyBudgetItemsInPeriod(final String organizationId, final int yearFrom, final int yearTo,
                                                                  final Map<BudgetItemReportType, List<String>> reportsAndCodes) {
        String restriction = "";
        if (reportsAndCodes != null) {
            restriction = reportsAndCodes.entrySet().stream()
                .map(n -> {
                    String query = "data->>'report' = '" + n.getKey().name() + "'";
                    if (n.getValue() != null && !n.getValue().isEmpty()) {
                        // list of codes to string with values surrounded by "'" and separated with ","
                        query += " AND data->>'level3Code'" +
                            " IN (" + n.getValue().stream().map(m -> "'" + m + "'").collect(Collectors.joining(",")) + ")";
                    }

                    return "(" + query + ")";
                }).collect(Collectors.joining(" OR "));
        }

        try {
            PreparedStatement statement = connection.prepareStatement(
                "SELECT * FROM " + getTableWithSchema() +
                " WHERE "
                    + (!restriction.isEmpty() ? "(" + restriction + ") AND " : "")
                    + "data#>>'{body,bodyIds,0,id}' = ? AND (data->>'year')::int BETWEEN ? AND ?");

            statement.setString(1, organizationId);
            statement.setInt(2, yearFrom);
            statement.setInt(3, yearTo);

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
