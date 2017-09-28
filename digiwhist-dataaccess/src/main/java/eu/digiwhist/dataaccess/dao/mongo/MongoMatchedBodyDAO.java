package eu.digiwhist.dataaccess.dao.mongo;

import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.tuple.Pair;
import org.mongojack.DBProjection;
import org.mongojack.DBQuery;

import eu.dl.dataaccess.dao.MatchedBodyDAO;
import eu.dl.dataaccess.dao.mongo.GenericMongoDAO;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.matched.MatchedGroupInfo;

/**
 * Matched body DAO implementation for MongoDB.
 */
class MongoMatchedBodyDAO extends GenericMongoDAO<MatchedBody> implements MatchedBodyDAO<MatchedBody> {
    private static final String MATCHED_BODY_COLLECTION_NAME = "matchedBody";

    @Override
    protected final Class<MatchedBody> getDTOClass() {
        return MatchedBody.class;
    }

    @Override
    protected final String getCollectionName() {
        return MATCHED_BODY_COLLECTION_NAME;
    }

    @Override
    public final MatchedBody getEmptyInstance() {
        return new MatchedBody();
    }

    @Override
    public final List<MatchedBody> getMineByHash(final String hash) {
        // build query with conditions for matchers
        DBQuery.Query matchersQuery = DBQuery.is("modifiedBy", workerName).is("modifiedByVersion", workerVersion);

        final List<MatchedBody> result = collection.find(DBQuery.and(DBQuery.is("hash", hash), matchersQuery),
                DBProjection.include("_id", "groupId")).toArray();

        return result;
    }

    @Override
    public List<MatchedBody> getByHash(final String hash) {
        // build query with conditions for matchers
        DBQuery.Query matchersQuery = DBQuery.is("modifiedBy", workerName).is("modifiedByVersion", workerVersion);
        for (Pair<String, String> matcher : additionalWorkers) {
            matchersQuery = DBQuery.or(matchersQuery,
                    DBQuery.is("modifiedBy", matcher.getKey()).is("modifiedByVersion", matcher.getValue()));
        }

        final List<MatchedBody> result = collection.find(DBQuery.and(DBQuery.is("hash", hash), matchersQuery),
                DBProjection.include("_id", "groupId")).toArray();

        return result;
    }

    @Override
    public final List<MatchedBody> getByGroupId(final String groupId) {
        final List<MatchedBody> result = collection.find(DBQuery.is("groupId", groupId)).toArray();

        return result;
    }

    @Override
    public final List<MatchedBody> getExactMatchBodiesPool(final String standardizedName,
            final String standardizedAddress, final List<BodyIdentifier> bodyIds) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public final List<MatchedBody> getApproximateMatchBodiesPool(final String standardizedName,
            final String standardizedAddress, final List<BodyIdentifier> bodyIds, final String digest) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public final MatchedBody getByEtalonId(final String id) {
        if (id == null) {
            return null;
        }

        DBQuery.Query matchersQuery = DBQuery.is("modifiedBy", workerName).is("modifiedByVersion", workerVersion);
        for (Pair<String, String> matcher : additionalWorkers) {
            matchersQuery = DBQuery.or(matchersQuery,
                    DBQuery.is("modifiedBy", matcher.getKey()).is("modifiedByVersion", matcher.getValue()));
        }

        final MatchedBody result = collection.find(DBQuery.and(matchersQuery,
                DBQuery.elemMatch("bodyIds", DBQuery.is("id", id).is("scope", BodyIdentifier.Scope.ETALON_ID)))).curr();

        return result;
    }

    @Override
    public final List<MatchedBody> getForResend(final String workerName, final String workerVersion) {
        return getMine(workerName, workerVersion, null, null);
    }
    
    @Override
    public List<MatchedGroupInfo> getGroupsInfo(final List<String> groups) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

	@Override
	public List<String> getEtalonGroupIds() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Map<String, String> getHashAndGroupIds() {
		// TODO Auto-generated method stub
		return null;
	}
}
