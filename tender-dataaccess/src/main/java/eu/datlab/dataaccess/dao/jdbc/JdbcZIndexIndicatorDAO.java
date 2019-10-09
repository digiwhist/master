package eu.datlab.dataaccess.dao.jdbc;

import eu.datlab.dataaccess.dao.ZIndexIndicatorDAO;
import eu.datlab.dataaccess.dto.codetables.ZIndexWhitelistRecordType;
import eu.datlab.dataaccess.dto.zindex.ZIndexIndicator;
import eu.datlab.dataaccess.dto.zindex.ZIndexOffense;
import eu.datlab.dataaccess.dto.zindex.ZIndexWhitelistRecord;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.jdbc.GenericJdbcDAO;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.List;

/**
 * JDBC DAO implementation for ZIndex indicators.
 * 
 * @author Tomas Mraze
 */
public final class JdbcZIndexIndicatorDAO extends GenericJdbcDAO<ZIndexIndicator> implements ZIndexIndicatorDAO {

    private static final String TABLE_NAME = "zindex_indicator";
    /**
     * Initializes connection etc.
     */
    public JdbcZIndexIndicatorDAO() {
        super();
    }

    @Override
    public ZIndexIndicator getEmptyInstance() {
        return new ZIndexIndicator();
    }

    @Override
    protected String getTableWithSchema() {
        return schema + "." + TABLE_NAME;
    }

    @Override
    public List<ZIndexIndicator> getByName(final String name) {
        try {
            PreparedStatement statement = connection.prepareStatement(
                "SELECT * FROM " + getTableWithSchema() + " WHERE data @> '{ \"name\":\"" + name + "\"}' ");

            ResultSet rs = statement.executeQuery();

            List<ZIndexIndicator> result = new ArrayList<>();
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
    public List<ZIndexIndicator> getAll() {
        try {
            PreparedStatement statement = connection.prepareStatement(
                "SELECT * FROM " + getTableWithSchema());

            ResultSet rs = statement.executeQuery();

            List<ZIndexIndicator> result = new ArrayList<>();
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
    public List<ZIndexOffense> getOffenses(final String organizationId, final Integer yearFrom, final Integer yearTo) {
        try {
            PreparedStatement statement = connection.prepareStatement(
                "SELECT * FROM " + schema + ".zindex_offense WHERE organizationid = ? AND year BETWEEN ? AND ?");

            statement.setString(1, organizationId);
            statement.setInt(2, yearFrom);
            statement.setInt(3, yearTo);

            ResultSet rs = statement.executeQuery();

            List<ZIndexOffense> result = new ArrayList<>();
            while (rs.next()) {
                result.add(new ZIndexOffense()
                    .setOrganizationId(rs.getString("organizationid"))
                    .setYear(rs.getInt("year"))
                    .setSubject(rs.getString("subject"))
                    .setIsSeriousOffense(rs.getBoolean("isseriousoffense"))
                    .setIsUOHSConfirmed(rs.getBoolean("isuohsconfirmed"))
                    .setUrl(rs.getString("url")));
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
    public List<ZIndexWhitelistRecord> getWhitelist(final ZIndexWhitelistRecordType type) {
        try {
            PreparedStatement statement = connection.prepareStatement(
                "SELECT * FROM " + schema + ".zindex_whitelist WHERE \"type\" = ?");

            statement.setString(1, type.name());

            ResultSet rs = statement.executeQuery();

            List<ZIndexWhitelistRecord> result = new ArrayList<>();
            while (rs.next()) {
                String recordType = rs.getString("type");

                result.add(new ZIndexWhitelistRecord()
                    .setType(recordType == null ? null : ZIndexWhitelistRecordType.valueOf(recordType))
                    .setUrl(rs.getString("url")));
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
