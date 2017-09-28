package eu.digiwhist.dataaccess.dao.jdbc;

import eu.dl.dataaccess.dao.MasterBodyDAO;
import eu.dl.dataaccess.dao.jdbc.GenericJdbcDAO;
import eu.dl.dataaccess.dto.master.MasterBody;

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
}
