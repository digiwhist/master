package eu.dl.dataaccess.dao.jdbc;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.List;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.ExchangeRatesDAO;
import eu.dl.dataaccess.dto.ExchangeRates;

/**
 * JDBC implementation of exchange rates dao.
 *
 */
public class JdbcExhangeRatesDAO extends GenericJdbcDAO<ExchangeRates> implements ExchangeRatesDAO<ExchangeRates> {
    
    private static final String TABLE_NAME = "exchange_rates";
    
    @Override
    public final ExchangeRates getByDate(final LocalDate date) {
        try {
            PreparedStatement statement = connection.prepareStatement(
                    "SELECT * FROM " + getTableWithSchema() + " WHERE data @> '{ \"date\":\"" +
                            sanitizeForJsonString(date.format(DateTimeFormatter.ISO_LOCAL_DATE)) + "\"}' ");

            ResultSet rs = statement.executeQuery();

            ExchangeRates result = null;

            while (rs.next()) {
                result = createFromResultSet(rs);
            }

            rs.close();
            statement.close();

            return result;
        } catch (Exception e) {
            logger.error("Unable to perform query, because of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }

    @Override
    public final List<ExchangeRates> findAll() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public final ExchangeRates getEmptyInstance() {
        return new ExchangeRates();
    }

    @Override
    protected final String getTableWithSchema() {
        return schema + "." + TABLE_NAME;
    }
}
