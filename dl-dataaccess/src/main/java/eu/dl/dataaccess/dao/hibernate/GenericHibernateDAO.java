package eu.dl.dataaccess.dao.hibernate;

import com.fasterxml.jackson.databind.ObjectWriter;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.GenericDAO;
import eu.dl.dataaccess.dto.JsonData;
import eu.dl.dataaccess.dto.StorableDTO;

import javax.persistence.Query;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import javax.persistence.TransactionRequiredException;

/**
 * Generic DAO to store objects into hibernate database.
 *
 * @param <T>
 *         DTO to be handled
 */
public abstract class GenericHibernateDAO<T extends StorableDTO> extends BaseHibernateDAO<T> implements GenericDAO<T> {

    /**
     * Page size used when returning list of items.
     */
    public static final Integer PAGE_SIZE = 1000;

    /**
     * Initialize connection to db.
     */
    protected GenericHibernateDAO() {
        super();
        mapper.registerModule(new HibernateJacksonModule());
    }

    @Override
    public final String save(final T t) {
        logger.debug("Saving {}", t.getClass());
        populateDtoWithMetadata(t);

        String jsonData;

        t.setData(null);

        try {
            ObjectWriter objectWriter = mapper.writerWithDefaultPrettyPrinter();
            byte[] tWritten = objectWriter.writeValueAsBytes(t);
            // Written json
            jsonData = new String(tWritten, "UTF-8");
        } catch (Exception e) {
            logger.error("Unable to serialize data to json exception {}", e);
            throw new UnrecoverableException("Unable to serialize data to json", e);
        }

        t.setData(new JsonData().setData(jsonData));

        entityManager.persist(t);

        String id = t.getId();

        logger.debug("Data {} saved with ID {}", t.getClass(), id);
        return t.getId();
    }

    @Override
    public final T getById(final String id) {
        T t = entityManager.find(getDTOClass(), id);
        return deserialize(t);
    }

    /**
     * Returns skeleton objects which has been stored by the particular version
     * of the crawler/downloader. The skeletons does have only id fileed in, the
     * rest is not beeing set.
     *
     * @param workerName
     *         downloader/crawler name
     * @param workerVersion
     *         downloader/crawler version
     * @param fromDate
     *         from date
     * @param toDate
     *         to date
     *
     * @return set of object with only one attribute id having set.
     */
    public final List<T> getMine(final String workerName, final String workerVersion, final String fromDate,
            final String toDate) {
        Query countQuery = entityManager.createQuery(
                "SELECT count(*) FROM " + getDTOClass().getName() + " e WHERE modifiedBy = :modifiedBy AND "
                        + "modifiedByVersion = :modifiedByVersion");
        countQuery.setParameter("modifiedByVersion", workerVersion);
        countQuery.setParameter("modifiedBy", workerName);
        Long count = ((Long) countQuery.getSingleResult());
        Integer page = 0;
        List<T> result = new ArrayList<T>();

        while ((page * PAGE_SIZE) < count) {
            Query q = entityManager.createQuery(
                    "SELECT e FROM " + getDTOClass().getName() + " e WHERE modifiedBy = :modifiedBy AND "
                            + "modifiedByVersion = :modifiedByVersion");
            q.setParameter("modifiedByVersion", workerVersion);
            q.setParameter("modifiedBy", workerName);
            q.setFirstResult(page * PAGE_SIZE);
            q.setMaxResults(PAGE_SIZE);

            List<T> pagedResult = (List<T>) q.getResultList();
            for (T item : pagedResult) {
                T emptyT = getEmptyInstance();
                emptyT.setId(item.getId());
                result.add(emptyT);
            }
            entityManager.clear();
            page++;
        }

        return result;
    }

    @Override
    public final List<T> getModifiedAfter(final LocalDateTime timestamp, final Integer page) {
        Integer firstResult = 0;
        if (page > 0) {
            firstResult = page * PAGE_SIZE;
        }

        Query q = entityManager.createQuery(
                "SELECT e FROM " + getDTOClass().getName() + " e " + "WHERE modified > :modifiedAfter " + "ORDER BY "
                        + "modified ASC");
        q.setFirstResult(firstResult);
        q.setMaxResults(PAGE_SIZE);
        q.setParameter("modifiedAfter", timestamp);

        List<T> result = (List<T>) q.getResultList();
        result.forEach(item -> deserialize(item));
        return result;
    }

    @Override
    public final List<T> getModifiedAfter(final LocalDateTime timestamp, final String modifiedBy, final Integer page) {
        Integer firstResult = 0;
        if (page > 0) {
            firstResult = page * PAGE_SIZE;
        }

        Query q = entityManager.createQuery(
                "SELECT e FROM " + getDTOClass().getName() + " e " + "WHERE modified > :modifiedAfter  AND modifiedBy"
                        + " = :modifiedBy ORDER BY modified ASC");
        q.setFirstResult(firstResult);
        q.setMaxResults(PAGE_SIZE);
        q.setParameter("modifiedAfter", timestamp);
        q.setParameter("modifiedBy", modifiedBy);

        List<T> result = (List<T>) q.getResultList();
        result.forEach(item -> deserialize(item));
        return result;
    }

    @Override
    public final List<String> getIdsBySourceAndVersion(final String name, final String version) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    /**
     * Returns empty instance.
     *
     * @return empty instance
     */
    public abstract T getEmptyInstance();

    /**
     * Prepares sql condition part for additional workers if there are any.
     *
     * @return SQL statement condition part to use for filtering data modified by additional workers or empty string
     * if no additional workers provided.
     */
    protected final String prepareAdditionalWorkersCondition() {
        if (additionalWorkers != null && !additionalWorkers.isEmpty()) {
            StringBuilder additionalWorkersRestriction = new StringBuilder();
            additionalWorkers.stream()
                    .forEach(worker -> additionalWorkersRestriction.append(" OR (modifiedBy = '")
                            .append(worker.getKey())
                            .append("' AND modifiedByVersion = '")
                            .append(worker.getValue())
                            .append("')"));
            return additionalWorkersRestriction.toString();
        }
        return "";
    }

    @Override
    public final Boolean removeById(final String id) {
        try {
            Query q = entityManager.createQuery("DELETE FROM " + getDTOClass().getName() + "WHERE id = :id");
            q.setParameter("id", id);
            q.executeUpdate();

            return true;
        } catch (IllegalStateException | TransactionRequiredException e) {
            return false;
        }
    }
}
