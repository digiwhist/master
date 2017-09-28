package eu.digiwhist.dataaccess.dao.jdbc;

import eu.dl.dataaccess.dao.MasterTenderDAO;
import eu.dl.dataaccess.dao.jdbc.GenericJdbcDAO;
import eu.dl.dataaccess.dto.master.MasterTender;

/**
 * JDBC implementation of master tender DAO.
 */
public class JdbcMasterTenderDAO extends GenericJdbcDAO<MasterTender> implements MasterTenderDAO<MasterTender> {

    private static final String TABLE_NAME = "master_tender";

    @Override
    public final MasterTender getEmptyInstance() {
        return new MasterTender();
    }

    @Override
    protected final String getTableWithSchema() {
        return schema + "." + TABLE_NAME;
    }
}
