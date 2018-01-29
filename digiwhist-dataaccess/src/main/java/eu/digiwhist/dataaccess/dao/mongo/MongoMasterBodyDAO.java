package eu.digiwhist.dataaccess.dao.mongo;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.MasterBodyDAO;
import eu.dl.dataaccess.dao.mongo.GenericMongoDAO;
import eu.dl.dataaccess.dto.master.MasterBody;
import org.mongojack.DBQuery;

import java.util.Collection;
import java.util.List;

/**
 * Mastered body DAO implementation for MongoDB.
 */
class MongoMasterBodyDAO extends GenericMongoDAO<MasterBody> implements MasterBodyDAO<MasterBody> {
    private static final String MASTERED_TENDER_COLLECTION_NAME = "masterBody";

    @Override
    protected final Class<MasterBody> getDTOClass() {
        return MasterBody.class;
    }

    @Override
    protected final String getCollectionName() {
        return MASTERED_TENDER_COLLECTION_NAME;
    }

    @Override
    public final boolean existsInPoliticalExposedPersons(final String bvdIdNumber) {
        throw new UnrecoverableException("Unsupported feature");
    }

    @Override
    public final List<MasterBody> getByGroupId(final String groupId) {
        final List<MasterBody> result = collection.find(DBQuery.is("groupId", groupId)).toArray();

        return result;
    }

    @Override
    public MasterBody getEmptyInstance() {
        return new MasterBody();
    }

    @Override
    public final List<MasterBody> getByGroupIds(final Collection<String> groupIds) {
        return null;
    }
}
