package eu.digiwhist.dataaccess.dao.mongo;

import eu.dl.dataaccess.dao.MatchedTenderDAO;
import eu.dl.dataaccess.dao.mongo.GenericMongoDAO;
import eu.dl.dataaccess.dto.matched.MatchedTender;
import org.apache.commons.lang3.tuple.Pair;
import org.mongojack.DBProjection;
import org.mongojack.DBQuery;

import java.net.URL;
import java.util.ArrayList;
import java.util.List;

/**
 * Matched tender DAO implementation for MongoDB.
 */
class MongoMatchedTenderDAO extends GenericMongoDAO<MatchedTender> implements MatchedTenderDAO<MatchedTender> {
    private static final String MATCHED_BODY_COLLECTION_NAME = "matchedTender";

    @Override
    protected final Class<MatchedTender> getDTOClass() {
        return MatchedTender.class;
    }

    @Override
    protected final String getCollectionName() {
        return MATCHED_BODY_COLLECTION_NAME;
    }

    @Override
    public final MatchedTender getEmptyInstance() {
        return new MatchedTender();
    }

    @Override
    public final List<MatchedTender> getMineByHash(final String hash) {
        // build query with conditions for matchers
        DBQuery.Query matchersQuery = DBQuery.is("modifiedBy", workerName).is("modifiedByVersion", workerVersion);

        final List<MatchedTender> result = collection.find(DBQuery.and(DBQuery.is("hash", hash), matchersQuery),
                DBProjection.include("_id", "groupId")).toArray();

        return result;
    }

    @Override
    public List<MatchedTender> getByHash(final String hash) {
        // build query with conditions for matchers
        DBQuery.Query matchersQuery = DBQuery.is("modifiedBy", workerName).is("modifiedByVersion", workerVersion);
        for (Pair<String, String> matcher : additionalWorkers) {
            matchersQuery = DBQuery.or(matchersQuery,
                    DBQuery.is("modifiedBy", matcher.getKey()).is("modifiedByVersion", matcher.getValue()));
        }

        final List<MatchedTender> result = collection.find(DBQuery.and(DBQuery.is("hash", hash), matchersQuery),
                DBProjection.include("_id", "groupId")).toArray();

        return result;
    }

    @Override
    public final List<MatchedTender> getByGroupId(final String groupId) {
        final List<MatchedTender> result = collection.find(DBQuery.is("groupId", groupId)).toArray();

        return result;
    }

    @Override
    public final List<MatchedTender> getByPublicationSourceIds(final List<String> publicationSourceIds) {

        final List<MatchedTender> result = collection.find(DBQuery.is("modifiedBy", workerName)
                .is("modifiedByVersion", workerVersion)
                .in("publications.sourceId", publicationSourceIds)).toArray();

        return (result.isEmpty() ? null : result);
    }

    @Override
    public final List<MatchedTender> getByPublicationHumanReadableUrls(final List<URL> humanReadableUrls) {
        // todo: the query below works only for one URL. It needs to be refactored now
        //        final List<MatchedTender> result = collection
        //                .find(DBQuery.is("modifiedBy", workerName).is("modifiedByVersion", workerVersion)
        //                                .is("publications.humanReadableUrl", humanReadableUrl),
        //                        DBProjection.include("_id", "groupId"))
        //                .toArray();
        //
        //        return result;

        return new ArrayList<>();
    }

    @Override
    public final List<MatchedTender> getByPublicationMachineReadableUrls(final List<URL> machineReadableUrls) {
        // todo: the query below works only for one URL. It needs to be refactored now
        //        final List<MatchedTender> result = collection
        //                .find(DBQuery.is("modifiedBy", workerName).is("modifiedByVersion", workerVersion)
        //                                .is("publications.machineReadableUrl", machineReadableUrl),
        //                        DBProjection.include("_id", "groupId"))
        //                .toArray();
        //
        //        return result;

        return new ArrayList<>();
    }

    @Override
    public final List<MatchedTender> getByDocumentsUrl(final URL documentsUrl) {
        // it is not implemented
        return new ArrayList<>();
    }

    @Override
    public final List<MatchedTender> getForResend(final String workerName, final String workerVersion) {
        return getMine(workerName, workerVersion, null, null);
    }
}
