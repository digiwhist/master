package eu.datlab.dataaccess.dao.jdbc;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.MasterBodyDAO;
import eu.dl.dataaccess.dao.jdbc.GenericJdbcDAO;
import eu.dl.dataaccess.dto.master.MasterBody;

import java.sql.PreparedStatement;
import java.sql.ResultSet;

/**
 * JDBC implementation of master tender DAO.
 */
public class JdbcMasterBodyDAO extends GenericJdbcDAO<MasterBody> implements MasterBodyDAO<MasterBody> {

    private static final String TABLE_NAME = "master_body";

    @Override
    public final MasterBody getEmptyInstance() {
        return new MasterBody();
    }

    @Override
    protected final String getTableWithSchema() {
        return schema + "." + TABLE_NAME;
    }

    /**
     * Find out if BVD id is in table political exposed persons.
     *
     * @param bvdIdNumber bvd id number to search for
     *
     * @return boolean
     */
    public final boolean existsInPoliticalExposedPersons(final String bvdIdNumber) {
        try {
            PreparedStatement statement = connection.prepareStatement(
                    "SELECT * FROM " + schema + ".political_exposed_persons WHERE mi_bvd_id_number = '" +
                            bvdIdNumber + "'");

            ResultSet rs = statement.executeQuery();

            boolean result = rs.next();

            rs.close();
            statement.close();

            return result;
        } catch (Exception e) {
            logger.error("Unable to perform query, because of of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }
}
