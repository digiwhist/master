package eu.datlab.dataaccess.dao.jdbc;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.ManualMatchDAO;
import eu.dl.dataaccess.dao.jdbc.BaseJdbcDAO;
import eu.dl.dataaccess.dto.matched.ManualMatch;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * JDBC implementation of manual match.
 * 
 * @param <T>
 *            manual match class
 */
public class JdbcManualMatchDAO<T extends ManualMatch> extends BaseJdbcDAO<ManualMatch>
        implements ManualMatchDAO<ManualMatch> {

    private static final String TABLE_NAME = "manual_match";

    @Override
    public final List<ManualMatch> getByHash(final String hash, final String flag) {
        if (hash == null) {
            return Collections.emptyList();
        }

        try {
            String query;
            if (flag != null) {
                query = "data @> '{\"hash\":\"" + sanitizeForJsonString(hash) + "\","
                    + "\"flag\":\"" + sanitizeForJsonString(flag) + "\"}'";
            } else {
                query = "data @> '{\"hash\":\"" + sanitizeForJsonString(hash) + "\"}' AND data->'flag' IS NULL";
            }

            PreparedStatement statement =
                getConnection().prepareStatement("SELECT * FROM " + getTableWithSchema() + " WHERE " + query);

            ResultSet rs = statement.executeQuery();

            List<ManualMatch> result = new ArrayList<>();

            while (rs.next()) {
                ManualMatch mm = new ManualMatch();
                mm.setFullHash(rs.getString(1));
                mm.setFlag(rs.getString(2));
                mm.setGroupId(rs.getString(3));
                result.add(mm);
            }

            rs.close();
            statement.close();

            return result;
        } catch (Exception e) {
            logger.error("Unable to generate result set, because of of {}", e);
            throw new UnrecoverableException("Unable to generate result set.", e);
        }
    }

    /**
     * Created <schema>.<table_name> string used by statements.
     * 
     * @return <schema>.<table_name> string
     */
    protected final String getTableWithSchema() {
        return schema + "." + TABLE_NAME;
    }
    
    @Override
	public final List<ManualMatch> getAllEntries(final String flag) {
		if (flag == null) {
            return Collections.emptyList();
        }
        
        try {
//            String query = "data @> '{\"flag\":\"" + sanitizeForJsonString(flag) + "\"}'";

            PreparedStatement statement =
                getConnection().prepareStatement("SELECT * FROM " + getTableWithSchema() + " WHERE flag = ?");

            statement.setString(1, flag);
            ResultSet rs = statement.executeQuery();

            List<ManualMatch> result = new ArrayList<>();

            while (rs.next()) {
                ManualMatch mm = new ManualMatch();
                mm.setFullHash(rs.getString("hash"));
                mm.setFlag(rs.getString("flag"));
                mm.setGroupId(rs.getString("groupid"));
                result.add(mm);
            }

            rs.close();
            statement.close();

            return result;
        } catch (Exception e) {
            logger.error("Unable to generate result set, because of of {}", e);
            throw new UnrecoverableException("Unable to generate result set.", e);
        }
	}
}