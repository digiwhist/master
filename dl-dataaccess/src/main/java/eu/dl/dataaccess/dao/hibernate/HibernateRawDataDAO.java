package eu.dl.dataaccess.dao.hibernate;

import eu.dl.dataaccess.dao.RawDataDAO;
import eu.dl.dataaccess.dto.raw.RawData;

/**
 * Hibernate implementation of raw data DAO.
 */
public class HibernateRawDataDAO extends GenericHibernateDAO<RawData>
        implements RawDataDAO<RawData> {

    @Override
    protected final Class<RawData> getDTOClass() {
        return RawData.class;
    }

    @Override
    public final RawData getEmptyInstance() {
        return new RawData();
    }

    @Override
    public final RawData getBySourceUrl(final String name, final String version, final String sourceUrl) {
        throw new UnsupportedOperationException("Not supported yet.");
    }
}
