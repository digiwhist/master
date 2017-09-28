package eu.dl.dataaccess.dao.mongo;

import org.mongojack.internal.MongoJackModule;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.mongodb.DB;
import com.mongodb.MongoClient;

import eu.dl.core.config.Config;

/**
 * Mongo Connector singleton for connecting to MongoDB database.
 */
public enum MongoConnector {
    /**
     * Holds the instance.
     */
    INSTANCE;

    private final DB database;

    private final ObjectMapper mapper;

    /**
     * Main logger.
     */
    private final Logger logger = LoggerFactory.getLogger(this.getClass().getName());

    /**
     * Initialization of the class.
     */
    MongoConnector() {
        // init db connection
        final String mongoHost = Config.getInstance().getParam("mongo.host");
        final Integer mongoPort = Integer.parseInt(Config.getInstance().getParam("mongo.port"));
        logger.debug("Connecting to mongo db {}:{}", mongoHost, mongoPort);
        final MongoClient mongoClient = new MongoClient(mongoHost, mongoPort);
        logger.info("Connection established with mongo db {}:{}", mongoHost, mongoPort);
        // get db
        database = mongoClient.getDB(Config.getInstance().getParam("mongo.dbname"));

        // init object mapper for mongojack
        // java time module must be registered to support java 8 date time classes
        mapper = new ObjectMapper();
        mapper.registerModule(new JavaTimeModule());
        mapper.enable(DeserializationFeature.USE_BIG_DECIMAL_FOR_FLOATS);
        MongoJackModule.configure(mapper);
        MongoJacksonModule mongoModule = new MongoJacksonModule();
        mapper.registerModule(mongoModule);
    }

    /**
     * Returns instance of Mongo connector.
     *
     * @return initialized instance
     */
    public static MongoConnector getInstance() {
        return INSTANCE;
    }

    /**
     * Provides initialized MongoDb connection.
     *
     * @return new database connection
     */
    public DB getDatabase() {
        return database;
    }

    /**
     * @return object mapper ready to use with mongojack for mapping POJO to MongoDb
     */
    public ObjectMapper getObjectMapper() {
        return mapper;
    }
}
