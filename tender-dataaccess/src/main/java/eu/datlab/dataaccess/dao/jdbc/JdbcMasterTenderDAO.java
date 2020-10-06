package eu.datlab.dataaccess.dao.jdbc;

import eu.dl.dataaccess.dao.MasterTenderApiDAO;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.jdbc.GenericJdbcDAO;
import eu.dl.dataaccess.dto.master.MasterTender;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * JDBC implementation of master tender DAO.
 */
public class JdbcMasterTenderDAO extends GenericJdbcDAO<MasterTender> implements MasterTenderApiDAO {

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
                                                     final Integer page, final boolean opentender, final Integer pageSize) {
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
            PreparedStatement statement = getConnection().prepareStatement(query);

            statement.setTimestamp(1, Timestamp.valueOf(timestamp));
            statement.setInt(2, pageSize);
            statement.setInt(3, page * pageSize);

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

    @Override
    public final List<MasterTender> getModifiedAfter(final LocalDateTime timestamp, final String createdBy, final String countryCode,
                                                     final Integer page, final boolean opentender) {
        return getModifiedAfter(timestamp, createdBy, countryCode, page, opentender, getPageSize());
    }

    @Override
    public final List<MasterTender> getModifiedAfterForBuyerProfileMatching(final LocalDateTime timestamp, final String createdBy,
                                                                            final Integer page, final Integer pageSize) {
        if (createdBy == null) {
            return Collections.emptyList();
        }

        try {
            PreparedStatement statement = getConnection().prepareStatement(
                "WITH data AS ("
                    + "SELECT id,"
                        + " min(p->>'publicationDate') FILTER (WHERE p->>'publicationDate' IS NOT NULL and (p->>'isIncluded')::boolean)"
                            + " AS min_publication_date"
                    + " FROM " + getTableWithSchema() + " t, jsonb_array_elements(t.data->'publications') p"
                    + " WHERE createdby = ? AND modified > ? AND data@>'{\"publications\": [{\"isIncluded\": true}]}'"
                    + " GROUP BY id"
                + ")"
                + " SELECT *"
                + " FROM " + getTableWithSchema()
                + " WHERE id IN (SELECT id FROM data WHERE min_publication_date IS NULL OR min_publication_date >= '2012-06-30')"
                + " ORDER BY modified ASC LIMIT ? OFFSET ?"
            );

            Integer size = pageSize != null ? pageSize : getPageSize();

            statement.setString(1, createdBy);
            statement.setTimestamp(2, Timestamp.valueOf(timestamp));
            statement.setInt(3, size);
            statement.setInt(4, page == null ? 0 : page * size);

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
