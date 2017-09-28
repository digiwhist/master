package eu.dl.dataaccess.dao.jdbc;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.PlainDocumentDAO;
import eu.dl.dataaccess.dto.PlainDocument;

import java.sql.PreparedStatement;
import java.sql.ResultSet;

/**
 * Jdbc DAO implementation for plain document.
 */
public class JdbcPlainDocumentDAO extends GenericJdbcDAO<PlainDocument> implements PlainDocumentDAO {
    private static final String TABLE_NAME = "plain_document";

    @Override
    protected final String getTableWithSchema() {
        return schema + "." + TABLE_NAME;
    }

    @Override
    public final PlainDocument getEmptyInstance() {
        return new PlainDocument();
    }

    @Override
    public final PlainDocument getLastByHash(final String hash) {
        try {
            PreparedStatement statement = connection.prepareStatement(
                    "SELECT * FROM " + getTableWithSchema() + " WHERE data @> '{ \"hash\":\"" + sanitize(
                            hash) + "\"}'");

            statement.executeQuery();

            ResultSet rs = statement.executeQuery();

            PlainDocument result = null;

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
}
