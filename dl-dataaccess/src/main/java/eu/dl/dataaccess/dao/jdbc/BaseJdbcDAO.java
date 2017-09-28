package eu.dl.dataaccess.dao.jdbc;

import java.sql.Connection;
import java.text.SimpleDateFormat;
import java.util.List;

import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;

import eu.dl.core.config.Config;
import eu.dl.dataaccess.dao.BaseDAO;

/**
 * Basic JDBC DAO implementation.
 *
 * @param <T>
 *         DTO to be handled
 *
 * @author Kuba Krafka
 */
public abstract class BaseJdbcDAO<T> implements BaseDAO<T> {
    protected final Logger logger = LoggerFactory.getLogger(this.getClass());

    protected Config config;

    protected String workerName;

    protected String workerVersion;

    protected List<Pair<String, String>> additionalWorkers;

    protected final Connection connection;

    protected final ObjectMapper mapper;

    protected final String schema;

    /**
     * Page size used in paged methods.
     */
    public static final Integer PAGE_SIZE = 1000;

    /**
     * Initializes connection etc.
     */
    protected BaseJdbcDAO() {
        config = Config.getInstance();

        schema = config.getParam("jdbc.schema");

        connection = JdbcTransactionUtils.getInstance().getConnection();

        mapper = new ObjectMapper();
        mapper.enable(DeserializationFeature.USE_BIG_DECIMAL_FOR_FLOATS);
        mapper.registerModule(new JavaTimeModule());
        mapper.setDateFormat(new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'"));
        mapper.setSerializationInclusion(Include.NON_NULL);
        mapper.configure(SerializationFeature.WRITE_NULL_MAP_VALUES, false);

        mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
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
    public final BaseJdbcDAO<T> populateWithWorkerMetadata(final String name, final String version) {
        setWorkerName(name);
        setWorkerVersion(version);
        return this;
    }

    /**
     * @return the workerName
     */
    protected final String getWorkerName() {
        return workerName;
    }

    /**
     * @param workerName
     *         the workerName to set
     */
    public final void setWorkerName(final String workerName) {
        this.workerName = workerName;
    }

    /**
     * @return the workerVersion
     */
    protected final String getWorkerVersion() {
        return workerVersion;
    }

    /**
     * @param workerVersion
     *         the workerVersion to set
     */
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
    public final BaseJdbcDAO setAdditionalWorkers(final List<Pair<String, String>> additionalWorkers) {
        this.additionalWorkers = additionalWorkers;
        return this;
    }

    /**
     * Escapes string to be usable in statements.
     *
     * @param entry
     *         string to be sanitized
     *
     * @return sanitized string
     */
    protected final String sanitize(final String entry) {
        return entry.replace("'", "''");
    }

    /**
     * Escapes string to be usable in statements.
     *
     * @param entry
     *         string to be sanitized
     *
     * @return sanitized string
     */
    protected final String sanitizeForJsonString(final String entry) {
        String result = new String("");
    		if (entry != null) {
        		// fix single quotes (postgres)
        		result = entry.replace("'", "''");
    		
        		// escapes JSON special characters (", \, /, \b, \f, \n, \r, \t, unicode)
        		result = StringEscapeUtils.escapeJson(result);
        }
        return result;
    }
}
