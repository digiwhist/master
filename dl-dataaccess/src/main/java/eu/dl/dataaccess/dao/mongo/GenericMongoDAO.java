package eu.dl.dataaccess.dao.mongo;

import java.time.LocalDateTime;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

import org.apache.commons.lang3.tuple.Pair;
import org.mongojack.DBCursor;
import org.mongojack.DBProjection;
import org.mongojack.DBQuery;
import org.mongojack.DBUpdate;
import org.mongojack.JacksonDBCollection;
import org.mongojack.WriteResult;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mongodb.BasicDBObject;
import com.mongodb.BasicDBObjectBuilder;
import com.mongodb.DB;

import eu.dl.core.config.Config;
import eu.dl.dataaccess.dto.StorableDTO;

/**
 * Generic DAO to store objects into mongo database.
 *
 * @param <T>
 *         one of DTOs
 *
 * @author Kuba Krafka
 */
public abstract class GenericMongoDAO<T> implements MongoDAO<T> {

    protected final Logger logger = LoggerFactory.getLogger(this.getClass());

    protected final JacksonDBCollection<T, String> collection;

    protected final JacksonDBCollection<T, String> deletedCollection;

    protected String workerName;

    protected String workerVersion;

    protected List<Pair<String, String>> additionalWorkers;
    
    /**
     * Application config instance.
     */
    protected final Config config;

    /**
     * Page size used when returning list of items.
     */
    public static final Integer PAGE_SIZE = 1000;

    /**
     * Initializes connection to db.
     */
    protected GenericMongoDAO() {
        final DB db = MongoConnector.getInstance().getDatabase();
        collection = JacksonDBCollection.wrap(db.getCollection(getCollectionName()), getDTOClass(), String.class,
                MongoConnector.getInstance().getObjectMapper());

        deletedCollection = JacksonDBCollection.wrap(db.getCollection(getCollectionName() + "Deleted"), getDTOClass(),
                String.class,
                MongoConnector.getInstance().getObjectMapper());

        config = Config.getInstance();
    }

    @Override
    public final String save(final T t) {
        logger.debug("Saving data into {}", getCollectionName());
        populateDtoWithMetadata(t);
        final WriteResult<T, String> result = collection.save(t);
        final String savedId = result.getSavedId();
        logger.debug("Data saved into {} as {}", getCollectionName(), savedId);
        return savedId;
    }

    @Override
    public final T getById(final String id) {
        return collection.findOneById(id);
    }

    @Override
    public final List<T> getMine(final String name, final String version, final String fromDate, final String toDate) {
        final List<T> result = collection.find(DBQuery.is("modifiedBy", name).is("modifiedByVersion", version),
                DBProjection.include("_id", "groupId")).toArray();

        return result;
    }

    @Override
    public final List<T> getDeletedAfter(final LocalDateTime timestamp, final Integer page) {
        final BasicDBObject whereQuery = new BasicDBObject();
        whereQuery.put("modified", BasicDBObjectBuilder.start("$gte", timestamp).get());
        final BasicDBObject sort = new BasicDBObject("modified", 1);

        // calculate how many records should be skipped
        Integer skip = PAGE_SIZE * (page - 1);
        if (skip < 0) {
            // don't skip backwards, set skip to zero
            skip = 0;
        }

        final List<T> result = deletedCollection.find(whereQuery).sort(sort).skip(skip).limit(PAGE_SIZE)
                .toArray();

        return result;
    }

    @Override
    public final void delete(final String id) {
        T deletedItem = collection.findOneById(id);
        populateDtoWithMetadata(deletedItem);

        // store in the deleted collection
        deletedCollection.insert(deletedItem);

        // delete from old collection
        collection.removeById(id);
    }

    @Override
    public final T getRandom(final String source) {
        final BasicDBObject whereQuery = new BasicDBObject();
        whereQuery.put("modifiedBy", source);

        final Integer count = collection.find(whereQuery).count();
        Integer skip = 0;
        if (count > 2) {
            skip = ThreadLocalRandom.current().nextInt(0, count);
        }

        final DBCursor<T> cursor = collection.find(whereQuery).skip(skip).limit(1);

        return cursor.next();
    }

    @Override
    public final List<T> getModifiedAfter(final LocalDateTime timestamp, final String modifiedBy, final Integer page) {
        final BasicDBObject whereQuery = new BasicDBObject();
        whereQuery.put("modified", BasicDBObjectBuilder.start("$gte", timestamp).get());
        whereQuery.put("modifiedBy", modifiedBy);
        final BasicDBObject sort = new BasicDBObject("modified", 1);

        // calculate how many records should be skipped
        Integer skip = PAGE_SIZE * (page - 1);
        if (skip < 0) {
            // don't skip backwards, set skip to zero
            skip = 0;
        }

        return collection.find(whereQuery).sort(sort).skip(skip).limit(PAGE_SIZE).toArray();
    }

    @Override
    public final List<T> getModifiedAfter(final LocalDateTime timestamp, final Integer page) {
        final BasicDBObject whereQuery = new BasicDBObject();
        whereQuery.put("modified", BasicDBObjectBuilder.start("$gte", timestamp).get());
        final BasicDBObject sort = new BasicDBObject("modified", 1);

        // calculate how many records should be skipped
        Integer skip = PAGE_SIZE * (page - 1);
        if (skip < 0) {
            // don't skip backwards, set skip to zero
            skip = 0;
        }

        return collection.find(whereQuery).sort(sort).skip(skip).limit(PAGE_SIZE)
                .toArray();
    }

    /**
     * Populates the dto with metadata.
     *
     * @param t
     *         dto to be populated
     */
    private void populateDtoWithMetadata(final T t) {
        final StorableDTO dto = (StorableDTO) t;
        dto.setCreatedBy(getWorkerName());
        dto.setCreatedByVersion(getWorkerVersion());
        dto.setModifiedBy(getWorkerName());
        dto.setModifiedByVersion(getWorkerVersion());
        final LocalDateTime now = LocalDateTime.now();
        dto.setCreated(now);
        dto.setModified(now);
    }

    /**
     * Populates DAO with worker info needed for metadata.
     *
     * @param name
     *         name of the worker
     * @param version
     *         version of the worker
     *
     * @return initialized dao
     */
    public final GenericMongoDAO<T> populateWithWorkerMetadata(final String name, final String version) {
        setWorkerName(name);
        setWorkerVersion(version);
        return this;
    }

    /**
     * Populates update builder with metadata.
     *
     * @param updateBuilder
     *         update builder object to be populated with metadata
     */
    protected final void populateUpdateWithMetadata(final DBUpdate.Builder updateBuilder) {
        updateBuilder.set("modified", LocalDateTime.now())
                .set("modifiedBy", getWorkerName())
                .set("modifiedByVersion", getWorkerVersion());
    }

    /**
     * Class of the DTO currently being worked with.
     *
     * @return class of the DTO
     */
    protected abstract Class<T> getDTOClass();

    /**
     * Returns collection name used to store DTOs.
     *
     * @return collection name
     */
    protected abstract String getCollectionName();

    /**
     * Returns collection that can be used to save/load objects to/from associated mongo collection.
     *
     * @return collection object
     */
    protected final JacksonDBCollection<T, String> getCollection() {
        return collection;
    }

    /**
     * @return the workerName
     */
    public final String getWorkerName() {
        return workerName;
    }

    /**
     * @param workerName
     *         the workerName to set
     */
    @Override
    public final void setWorkerName(final String workerName) {
        this.workerName = workerName;
    }

    /**
     * @return the workerVersion
     */
    public final String getWorkerVersion() {
        return workerVersion;
    }

    /**
     * @param workerVersion
     *         the workerVersion to set
     */
    @Override
    public final void setWorkerVersion(final String workerVersion) {
        this.workerVersion = workerVersion;
    }

    /**
     * Sets additionalWorkers.
     *
     * @param additionalWorkers
     *         the additionalWorkers to set
     *
     * @return this instance for chaining
     */
    public final GenericMongoDAO setAdditionalWorkers(final List<Pair<String, String>> additionalWorkers) {
        this.additionalWorkers = additionalWorkers;
        return this;
    }
}
