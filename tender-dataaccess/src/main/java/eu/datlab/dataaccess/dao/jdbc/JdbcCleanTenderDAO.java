package eu.datlab.dataaccess.dao.jdbc;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.CleanTenderDAO;
import eu.dl.dataaccess.dao.jdbc.GenericJdbcDAO;
import eu.dl.dataaccess.dto.clean.CleanTender;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;


/**
 * Hibernate DAO implementation for tenders.
 */
public class JdbcCleanTenderDAO extends GenericJdbcDAO<CleanTender> implements CleanTenderDAO<CleanTender> {
    private static final String TABLE_NAME = "clean_tender";

    @Override
    protected final String getTableWithSchema() {
        return schema + "." + TABLE_NAME;
    }

    @Override
    public final CleanTender getEmptyInstance() {
        return new CleanTender();
    }

    @Override
    public final List<String> getIncludedPublicationSourceIds(final LocalDate date) {
        try {
            PreparedStatement statement = getConnection().prepareStatement(
                    "SELECT p->>'sourceId' AS sourceId " +
                            "FROM " + getTableWithSchema() + " r, jsonb_array_elements(r.data->'publications') as p " +
                            "WHERE (createdBy = ? AND createdByVersion = ? AND p->>'isIncluded' = 'true')");

            statement.setString(1, workerName);
            statement.setString(2, workerVersion);

            ResultSet rs = statement.executeQuery();

            List<String> result = new ArrayList<>();

            while (rs.next()) {
                result.add(rs.getString("sourceId"));
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
