package eu.datlab.dataaccess.dao.jdbc;

import eu.dl.dataaccess.dao.MasterTenderOpentenderDAO;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.jdbc.GenericJdbcDAO;
import eu.dl.dataaccess.dto.master.MasterTender;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

/**
 * JDBC implementation of master tender DAO.
 */
public class JdbcMasterTenderDAO extends GenericJdbcDAO<MasterTender> implements MasterTenderOpentenderDAO {

    private static final String TABLE_NAME = "master_tender";

    @Override
    public final MasterTender getEmptyInstance() {
        return new MasterTender();
    }

    @Override
    protected final String getTableWithSchema() {
        return schema + "." + TABLE_NAME;
    }

    @Override
    public final List<MasterTender> getModifiedAfter(final LocalDateTime timestamp, final String createdBy, final String countryCode,
                                                     final Integer page, final boolean opentender) {
        try {
            String query = "SELECT * FROM " + getTableWithSchema() + " WHERE modified > ?";

            if (createdBy!= null && !createdBy.isEmpty()) {
                query = query + " AND createdby = '" + sanitize(createdBy) + "'";
            }

            if (countryCode!= null && !countryCode.isEmpty()) {
                query = query + " AND data->>'country' = '" + sanitize(countryCode) + "'";
            }

            if (opentender) {
                query = query + " AND data->'metaData'->>'opentender' = 'true'";
            }

            query = query + " ORDER BY modified ASC LIMIT ? OFFSET ?";

            System.out.println(query);
            PreparedStatement statement = connection.prepareStatement(query);

            statement.setTimestamp(1, Timestamp.valueOf(timestamp));
            statement.setInt(2, PAGE_SIZE);
            statement.setInt(3, page * PAGE_SIZE);

            ResultSet rs = statement.executeQuery();

            List<MasterTender> result = new ArrayList<>();

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
