package eu.digiwhist.dataaccess.dao.mongo;

import java.util.List;

import org.mongojack.DBProjection;
import org.mongojack.DBQuery;

import eu.dl.dataaccess.dao.ManualMatchDAO;
import eu.dl.dataaccess.dao.mongo.GenericMongoDAO;
import eu.dl.dataaccess.dto.matched.ManualMatch;

/**
 * Manual match DAO implementation for MongoDB.
 */
class MongoManualMatchDAO extends GenericMongoDAO<ManualMatch> implements ManualMatchDAO<ManualMatch> {
    private static final String MATCHED_BODY_COLLECTION_NAME = "manualMatch";

    @Override
    protected final Class<ManualMatch> getDTOClass() {
        return ManualMatch.class;
    }

    @Override
    protected final String getCollectionName() {
        return MATCHED_BODY_COLLECTION_NAME;
    }

    @Override
    public final List<ManualMatch> getByHash(final String hash, final String flag) {
        final List<ManualMatch> result = collection
                .find(DBQuery.is("modifiedBy", workerName).is("modifiedByVersion", workerVersion).is("flag", flag)
                        .is("hash", hash),
                        DBProjection.include("_id", "groupId"))
                .toArray();

        return result;
    }

	@Override
	public final List<ManualMatch> getAllEntries(final String flag) {
		// TODO Auto-generated method stub
		return null;
	}
}
