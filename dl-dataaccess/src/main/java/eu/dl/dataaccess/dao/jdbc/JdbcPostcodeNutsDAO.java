package eu.dl.dataaccess.dao.jdbc;

import eu.dl.core.UnrecoverableException;
import eu.dl.core.config.Config;
import eu.dl.dataaccess.dao.PostcodeNutsDAO;
import java.sql.Connection;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.List;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * JDBC implementation of postcode NUTS DAO.
 */
public final class JdbcPostcodeNutsDAO implements PostcodeNutsDAO {

    protected final Logger logger = LoggerFactory.getLogger(this.getClass());

    protected Config config;

    protected String schema;

    protected List<Pair<String, String>> additionalWorkers;

    protected final Connection connection;

    private static final String TABLE_NAME = "postcode_nuts";

    /**
     * Initializes connection etc.
     */
    public JdbcPostcodeNutsDAO() {
        config = Config.getInstance();

        schema = config.getParam("jdbc.schema");

        connection = JdbcTransactionUtils.getInstance().getConnection();
    }

    /**
     * @return table name with schema
     */
    private String getTableWithSchema() {
        return schema + "." + TABLE_NAME;
    }

    @Override
    public String getNutsByPostcode(final String postcode, final String country) {
        if (postcode == null || country == null) {
            return null;
        }

        try {
            PreparedStatement statement = connection.prepareStatement(
                "SELECT nuts" +
                " FROM " + getTableWithSchema() +
                " WHERE country = ? AND postcode = ?");

            statement.setString(1, country);
            statement.setString(2, postcode);

            ResultSet rs = statement.executeQuery();
            String nuts = null;
            // get the first record
            if (rs.next()) {
                nuts = rs.getString("nuts");
            }

            rs.close();
            statement.close();

            return nuts;
        } catch (Exception e) {
            logger.error("Unable to perform query, because of of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }
}