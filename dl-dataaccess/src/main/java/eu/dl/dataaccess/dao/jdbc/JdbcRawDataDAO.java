package eu.dl.dataaccess.dao.jdbc;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.RawDataDAO;
import eu.dl.dataaccess.dto.raw.RawData;

import java.sql.PreparedStatement;
import java.sql.ResultSet;

/**
 * JDBC implementation of raw data DAO.
 */
public class JdbcRawDataDAO extends GenericJdbcDAO<RawData> implements RawDataDAO<RawData> {

    private static final String TABLE_NAME = "raw_data";

    @Override
    public final RawData getEmptyInstance() {
        return new RawData();
    }

    @Override
    protected final String getTableWithSchema() {
        return schema + "." + TABLE_NAME;
    }

    @Override
    public final RawData getBySourceUrl(final String name, final String version, final String sourceUrl) {
        // CREATE INDEX WHEN YOU WANT TO USE THIS METHOD. The query is:
        // "CREATE INDEX raw_data_modifiedby_modifiedbyversion_data_sourceUrl_idx
        //   ON raw_data (modifiedby, modifiedbyversion, (data->>'sourceUrl'));"

        try {
            PreparedStatement statement = connection.prepareStatement(
                    "SELECT *" +
                            " FROM " + getTableWithSchema() +
                            " WHERE createdby = ? AND createdbyversion = ? AND data->>'sourceUrl' = ?");

            statement.setString(1, name);
            statement.setString(2, version);
            statement.setString(3, sourceUrl);

            ResultSet rs = statement.executeQuery();

            RawData result = null;

            // get the first record
            if (rs.next()) {
                result = createFromResultSet(rs);
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