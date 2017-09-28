package eu.dl.dataaccess.dao.mongo;

import eu.dl.dataaccess.dao.RawDataDAO;
import eu.dl.dataaccess.dto.raw.RawData;

import java.util.List;

/**
 * Abstract ancestor for all raw data DAO classes.
 */
public abstract class MongoRawDataDAO extends GenericMongoDAO<RawData> implements RawDataDAO<RawData> {

    @Override
    protected final Class<RawData> getDTOClass() {
        return RawData.class;
    }

    @Override
    public final RawData getEmptyInstance() {
        return new RawData();
    }

    @Override
    public final List<String> getIdsBySourceAndVersion(final String name, final String version) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public final RawData getBySourceUrl(final String name, final String version, final String sourceUrl) {
        throw new UnsupportedOperationException("Not supported yet.");
    }
}
