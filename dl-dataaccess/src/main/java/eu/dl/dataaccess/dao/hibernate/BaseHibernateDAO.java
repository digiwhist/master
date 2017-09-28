package eu.dl.dataaccess.dao.hibernate;

import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import eu.dl.core.UnrecoverableException;
import eu.dl.core.config.Config;
import eu.dl.dataaccess.dao.GenericDAO;
import eu.dl.dataaccess.dto.StorableDTO;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

/**
 * Generic DAO to store objects into hibernate database.
 *
 * @param <T>
 *         DTO to be handled
 */
public abstract class BaseHibernateDAO<T extends StorableDTO> implements GenericDAO<T> {
    protected final Logger logger = LoggerFactory.getLogger(this.getClass());

    protected String workerName;

    protected String workerVersion;

    protected List<Pair<String, String>> additionalWorkers;

    protected EntityManager entityManager;

    protected EntityManagerFactory entityManagerFactory;

    protected Config config;

    /**
     * Page size used when returning list of items.
     */
    public static final Integer PAGE_SIZE = 100;

    protected ObjectMapper mapper;

    /**
     * Initialize connection to db.
     */
    protected BaseHibernateDAO() {
        config = Config.getInstance();
        entityManager = HibernateTransactionUtils.getInstance().getEntityManager();

        mapper = new ObjectMapper();
        mapper.enable(DeserializationFeature.USE_BIG_DECIMAL_FOR_FLOATS);
        mapper.registerModule(new JavaTimeModule());
        mapper.setDateFormat(new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'"));
        mapper.setSerializationInclusion(Include.NON_NULL);
        mapper.configure(SerializationFeature.WRITE_NULL_MAP_VALUES, false);
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
    public final BaseHibernateDAO setAdditionalWorkers(final List<Pair<String, String>> additionalWorkers) {
        this.additionalWorkers = additionalWorkers;
        return this;
    }

    /**
     * Populates the dto with metadata.
     *
     * @param t
     *         dto to be populated
     */
    protected final void populateDtoWithMetadata(final T t) {
        final StorableDTO dto = (StorableDTO) t;
        final LocalDateTime now = LocalDateTime.now();

        if (dto.getCreated() == null) {
            dto.setCreatedBy(getWorkerName());
            dto.setCreatedByVersion(getWorkerVersion());
            dto.setCreated(now);
        }

        dto.setModifiedBy(getWorkerName());
        dto.setModifiedByVersion(getWorkerVersion());
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
    public final BaseHibernateDAO<T> populateWithWorkerMetadata(final String name, final String version) {
        setWorkerName(name);
        setWorkerVersion(version);
        return this;
    }

    /**
     * Recreates the data retrived from storage, updates all its value with data
     * stored in data column.
     *
     * @param t
     *         item to be deserialized
     *
     * @return deserialized object
     */
    protected final T deserialize(final T t) {
        try {
            mapper.readerForUpdating(t).readValue(t.getData().getData());
            logger.debug("Deserialized object {} with id {}", t, t.getId());
        } catch (IOException e) {
            logger.error("Unable to deserialize data from json exception {}", e);
            throw new UnrecoverableException("Unable to deserialize data from json", e);
        }

        return t;
    }

    /**
     * Recreates the data retrived from storage, updates all its value with data
     * stored in data column.
     *
     * @param list
     *         list of items to be deserialized
     *
     * @return deserialized object
     */
    protected final List<T> deserialize(final List<T> list) {
        List<T> result = new ArrayList<>();

        for (T item : list) {
            result.add(deserialize(item));
        }

        return result;
    }

    /**
     * Class of the DTO currently being worked with.
     *
     * @return class of the DTO
     */
    protected abstract Class<T> getDTOClass();

    /**
     * Escapes string to be usable in statements.
     *
     * @param entry
     *         string to be sanitized
     *
     * @return sanitized string
     */
    protected final String sanitize(final String entry) {
        return entry.replace("'", "\'");
    }
}
