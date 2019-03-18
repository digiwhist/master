package eu.dl.dataaccess.dao.jdbc;

import com.fasterxml.jackson.databind.ObjectWriter;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.GenericDAO;
import eu.dl.dataaccess.dto.StorableDTO;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;

import java.io.IOException;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * Generic DAO implementation for Jdbc connection.
 *
 * @param <T>
 *         class to be handled
 */
public abstract class GenericJdbcDAO<T extends StorableDTO> extends BaseJdbcDAO<T> implements GenericDAO<T> {

    /**
     * Gets tenders for a specific country.
     *
     * @param countryCode country code
     * @param page page
     * @return page with tenders
     */
    public final List<T> getByCountry(final String countryCode, final Integer page) {
        try {
            PreparedStatement statement = connection.prepareStatement(
                    "SELECT * FROM " + getTableWithSchema() + " WHERE data ->> 'country' = '" +
                            sanitizeForJsonString(countryCode) + "' ORDER BY modified ASC LIMIT ? " +
                            "OFFSET ?");

            statement.setInt(1, PAGE_SIZE);
            statement.setInt(2, page * PAGE_SIZE);
            ResultSet rs = statement.executeQuery();

            List<T> result = new ArrayList<T>();

            while (rs.next()) {
                result.add(createFromResultSet(rs));
            }

            rs.close();
            statement.close();

            return result;
        } catch (Exception e) {
            logger.error("Unable to perform query, because of of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }

    /**
     * Gets tenders for a specific country and source.
     *
     * @param countryCode country code
     * @param page page
     * @param createdBy createdBy
     * @return page with tenders
     */
    public final List<T> getByCountry(final String countryCode, final Integer page, final String createdBy) {
        try {
            PreparedStatement statement = connection.prepareStatement("SELECT * FROM " + getTableWithSchema()
                + " WHERE data ->> 'country' = '" + sanitizeForJsonString(countryCode) + "' AND createdby = ?"
                + " ORDER BY modified ASC LIMIT ? OFFSET ?");

            statement.setString(1, createdBy);
            statement.setInt(2, PAGE_SIZE);
            statement.setInt(3, page * PAGE_SIZE);
            ResultSet rs = statement.executeQuery();

            List<T> result = new ArrayList<>();
            while (rs.next()) {
                result.add(createFromResultSet(rs));
            }

            rs.close();
            statement.close();

            return result;
        } catch (Exception e) {
            logger.error("Unable to perform query, because of of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }


    /**
     * Returns items with the group id.
     *
     * @param groupId
     *         group id to be searched for
     *
     * @return list of items
     */
    public final List<T> getByGroupId(final String groupId) {
        try {
            PreparedStatement statement = connection.prepareStatement(
                    "SELECT * FROM " + getTableWithSchema() + " WHERE data @> '{ \"groupId\":\"" +
                            sanitizeForJsonString(
                            groupId) + "\"}' ");

            ResultSet rs = statement.executeQuery();

            List<T> result = new ArrayList<T>();

            while (rs.next()) {
                result.add(createFromResultSet(rs));
            }

            rs.close();
            statement.close();

            return result;
        } catch (Exception e) {
            logger.error("Unable to perform query, because of of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }

    /**
     * Returns items with the group id.
     *
     * @param groupIds
     *         group ids to be searched for
     *
     * @return list of items
     */
    public final List<T> getByGroupIds(final Collection<String> groupIds) {
        if (groupIds == null || groupIds.isEmpty()) {
            return null;
        }
        try {

            List<T> result = new ArrayList<T>();
            Integer size = groupIds.size();
            Integer counter = 0;
            Integer pageSize = 200;
            ArrayList<String> list = new ArrayList<String>(groupIds);

            while (counter < size) {
                String condition = new String();

                for (String groupId : list.subList(counter, Integer.min(size, counter + pageSize))) {
                    if (groupId != null) {
                        if (!condition.isEmpty()) {
                            condition = condition + " OR ";
                        }

                        condition = condition + "data->>'groupId' = '" + sanitizeForJsonString(groupId) + "' ";
                    }
                }

                PreparedStatement statement = connection.prepareStatement(
                        "SELECT * FROM " + getTableWithSchema() + " WHERE " + condition + " ORDER BY data->>'processingOrder';");

                ResultSet rs = statement.executeQuery();

                while (rs.next()) {
                    result.add(createFromResultSet(rs));
                }

                rs.close();
                statement.close();
                logger.debug("Selected {} bodies from {} to {}", pageSize, counter,
                        Integer.min(size, counter + pageSize));

                counter = counter + pageSize;
            }
            return result;
        } catch (Exception e) {
            logger.error("Unable to perform query, because of of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }

    @Override
    public final String save(final T t) {
        if (t != null) {
            try {
                PreparedStatement statement = null;

                // generate now stamp
                LocalDateTime now = LocalDateTime.now();
                Timestamp timestamp = Timestamp.valueOf(now);

                if (t.getId() == null) {
                    // insert
                    statement = connection.prepareStatement(
                            "INSERT INTO " + getTableWithSchema() + " (id, created, createdBy, createdByVersion, " +
                                    "modified, modifiedBy, modifiedByVersion, data)" + " VALUES (?, ?, ?, ?, ?, ?, ?,"
                                    + "" + "" + "" + "" + "" + "" + "" + "" + " ?)",
                            Statement.RETURN_GENERATED_KEYS);

                    // generate id and populate data
                    t.setId(UUID.randomUUID().toString());
                    t.setCreatedBy(getWorkerName());
                    t.setCreatedByVersion(getWorkerVersion());
                    t.setCreated(timestamp.toLocalDateTime());
                    t.setModifiedBy(getWorkerName());
                    t.setModifiedByVersion(getWorkerVersion());
                    t.setModified(timestamp.toLocalDateTime());

                    statement.setString(1, t.getId());
                    statement.setTimestamp(2, timestamp);
                    statement.setString(3, getWorkerName());
                    statement.setString(4, getWorkerVersion());
                    statement.setTimestamp(5, timestamp);
                    statement.setString(6, getWorkerName());
                    statement.setString(7, getWorkerVersion());
                    statement.setString(8, serializeToJson(t));

                    // execute insert ad get primary key returned
                    statement.executeUpdate();
                    ResultSet rs = statement.getGeneratedKeys();
                    if (rs.next()) {
                        t.setId(rs.getString(1));
                    }
                } else {
                    // update
                    statement = connection.prepareStatement(
                            "UPDATE " + getTableWithSchema() + " SET modified = ? , modifiedBy = ?, " +
                                    "modifiedByVersion" + " = ?, data = ? WHERE id = ?;");

                    // populate data for json
                    t.setModifiedBy(getWorkerName());
                    t.setModifiedByVersion(getWorkerVersion());
                    t.setModified(timestamp.toLocalDateTime());

                    statement.setTimestamp(1, timestamp);
                    statement.setString(2, getWorkerName());
                    statement.setString(3, getWorkerVersion());
                    statement.setString(4, serializeToJson(t));
                    statement.setString(5, t.getId());

                    // execute insert ad get primary key returned
                    statement.executeUpdate();
                }
                return t.getId();
            } catch (SQLException e) {
                logger.error("Unable to perform query, because of {}", e);
                throw new UnrecoverableException("Unable to perform query.", e);
            }
        } else {
            logger.error("Unable to save null");
            throw new UnrecoverableException("Unable to save null!");
        }
    }

    @Override
    public final T getById(final String id) {
        try {
            PreparedStatement statement = connection.prepareStatement(
                    "SELECT * FROM " + getTableWithSchema() + " WHERE id = ?");

            statement.setString(1, id);
            statement.executeQuery();

            ResultSet rs = statement.executeQuery();

            T result = null;

            while (rs.next()) {
                result = createFromResultSet(rs);
            }

            rs.close();
            statement.close();

            return result;
        } catch (Exception e) {
            logger.error("Unable to perform query, because of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }


    @Override
    public final List<T> getByIds(final List<String> ids) {
        if (ids == null || ids.isEmpty()) {
            return Collections.emptyList();
        }

        try {
            StringBuilder sql = new StringBuilder("SELECT * FROM " + getTableWithSchema() + " WHERE id IN (");

            sql.append(ids.stream()
                    .map(id -> "'" + sanitize(id) + "'")
                    .collect(Collectors.joining(",")));

            sql.append(");");

            PreparedStatement statement = connection.prepareStatement(sql.toString());

            statement.executeQuery();

            ResultSet rs = statement.executeQuery();

            List<T> result = new ArrayList<T>();

            while (rs.next()) {
                result.add(createFromResultSet(rs));
            }

            rs.close();
            statement.close();

            return result;
        } catch (Exception e) {
            logger.error("Unable to perform query, because of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }

    @Override
    public final List<T> getMine(final String name, final String version, final String fromDate, final String toDate) {
        try {
            PreparedStatement statement;
            if (fromDate != null && toDate != null) {
                statement = connection.prepareStatement(
                        "SELECT id FROM " + getTableWithSchema() + " WHERE createdby = ? AND createdbyversion = ? "
                                + "AND modified >= ? AND modified <= ? ORDER BY data->>'processingOrder'");
                statement.setString(3, fromDate);
                statement.setString(4, toDate);
            } else if (fromDate != null) {
                statement = connection.prepareStatement(
                        "SELECT id FROM " + getTableWithSchema() + " WHERE createdby = ? AND createdbyversion = ? "
                                + "AND modified >= ? ORDER BY data->>'processingOrder'");
                statement.setString(3, fromDate);
            } else if (toDate != null) {
                statement = connection.prepareStatement(
                        "SELECT id FROM " + getTableWithSchema() + " WHERE createdby = ? AND createdbyversion = ? "
                                + "modified <= ? ORDER BY data->>'processingOrder'");
                statement.setString(3, toDate);
            } else {
                statement = connection.prepareStatement(
                        "SELECT id FROM " + getTableWithSchema() + " WHERE createdby = ? AND createdbyversion = ? " +
                                "ORDER BY data->>'processingOrder'");
            }

            statement.setString(1, name);
            statement.setString(2, version);

            ResultSet rs = statement.executeQuery();

            List<T> result = new ArrayList<T>();

            while (rs.next()) {
                T t = getEmptyInstance();
                t.setId((rs.getString("id")));
                result.add(t);
            }

            rs.close();
            statement.close();

            return result;
        } catch (Exception e) {
            logger.error("Unable to perform query, because of of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }

    /**
     * Returns objects with the same hash which have been stored by the particular version of the matcher.
     *
     * @param hash
     *         hash to be searched
     *
     * @return list of objects with the same hash
     */
    public final List<T> getMineByHash(final String hash) {
        try {
            PreparedStatement statement = connection.prepareStatement(
                    "SELECT * FROM " + getTableWithSchema() + " WHERE data @> '{ \"hash\":\"" + sanitize(
                            hash) + "\"}' AND createdBy = ? AND createdbyversion = ? ORDER BY data->>'processingOrder'");

            statement.setString(1, workerName);
            statement.setString(2, workerVersion);

            ResultSet rs = statement.executeQuery();

            List<T> result = new ArrayList<T>();

            while (rs.next()) {
                result.add(createFromResultSet(rs));
            }

            rs.close();
            statement.close();

            return result;
        } catch (Exception e) {
            logger.error("Unable to perform query, because of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }
    
    /**
     * Returns objects with which have been stored by the 
     * particular version of the matcher (or its relative).
     *
     * @param page
     * 		   no. of page (from 0)
     * 
     * @return list of objects with the same hash
     */
    public final List<T> getMine(final Integer page) {
        try {
            PreparedStatement statement = connection.prepareStatement(
                    "SELECT * FROM " + getTableWithSchema()
                    + " WHERE ((createdBy = ? AND createdByVersion = ?) "
        				+ prepareAdditionalWorkersCondition() + ")"
                    + " ORDER BY data->>'processingOrder' ASC LIMIT ? OFFSET ?");

            statement.setString(1, workerName);
            statement.setString(2, workerVersion);
            statement.setInt(3, PAGE_SIZE);
            statement.setInt(4, page * PAGE_SIZE);
            
            ResultSet rs = statement.executeQuery();

            List<T> result = new ArrayList<T>();

            while (rs.next()) {
                result.add(createFromResultSet(rs));
            }

            rs.close();
            statement.close();

            return result;
        } catch (Exception e) {
            logger.error("Unable to perform query, because of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }

    /**
     * Returns objects with the same hash which have been stored by specified workers (and versions of workers).
     *
     * @param hash
     *         hash to be searched
     *
     * @return list of objects with the same hash and stored be one of the specified workers
     */
    public final List<T> getByHash(final String hash) {
        // prepare sql condition part for additional workers
        String additionalWorkersCondition = prepareAdditionalWorkersCondition();

        try {
            PreparedStatement statement = connection.prepareStatement(
                    "SELECT * FROM " + getTableWithSchema() + " WHERE data @> '{ \"hash\":\"" + sanitize(
                            hash) + "\"}' AND ( (createdBy = ? AND createdByVersion = ?) " +
                            additionalWorkersCondition + ")");

            statement.setString(1, workerName);
            statement.setString(2, workerVersion);

            ResultSet rs = statement.executeQuery();

            List<T> result = new ArrayList<T>();

            while (rs.next()) {
                result.add(createFromResultSet(rs));
            }

            rs.close();
            statement.close();

            return result;
        } catch (Exception e) {
            logger.error("Unable to perform query, because of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }

    @Override
    public final List<T> getModifiedAfter(final LocalDateTime timestamp, final Integer page) {
        try {
            PreparedStatement statement = connection.prepareStatement(
                    "SELECT * FROM " + getTableWithSchema() + " WHERE modified > ? ORDER BY modified ASC LIMIT ? " +
                            "OFFSET ?");

            statement.setTimestamp(1, Timestamp.valueOf(timestamp));
            statement.setInt(2, PAGE_SIZE);
            statement.setInt(3, page * PAGE_SIZE);

            ResultSet rs = statement.executeQuery();

            List<T> result = new ArrayList<T>();

            while (rs.next()) {
                result.add(createFromResultSet(rs));
            }

            rs.close();
            statement.close();

            return result;
        } catch (Exception e) {
            logger.error("Unable to perform query, because of of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }

    @Override
    public final List<T> getModifiedAfter(final LocalDateTime timestamp, final String modifiedBy, final Integer page) {
        try {
            PreparedStatement statement = connection.prepareStatement(
                    "SELECT * FROM " + getTableWithSchema() + " WHERE modified > ? AND createdby = ? ORDER BY " +
                            "modified ASC LIMIT ? OFFSET ?");

            statement.setTimestamp(1, Timestamp.valueOf(timestamp));
            statement.setString(2, modifiedBy);
            statement.setInt(3, PAGE_SIZE);
            statement.setInt(4, page * PAGE_SIZE);

            ResultSet rs = statement.executeQuery();

            List<T> result = new ArrayList<T>();

            while (rs.next()) {
                result.add(createFromResultSet(rs));
            }

            rs.close();
            statement.close();

            return result;
        } catch (Exception e) {
            logger.error("Unable to perform query, because of of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }

    @Override
    public final List<T> getModifiedAfter(final LocalDateTime timestamp,
                                         final String createdBy,
                                         final String countryCode,
                                         final Integer page) {
        try {
            String query = "SELECT * FROM " + getTableWithSchema() + " WHERE modified > ? ";

            if (createdBy!= null && !createdBy.isEmpty()) {
                query = query + " AND createdby = '" + sanitize(createdBy) + "' ";
            }

            if (countryCode!= null && !countryCode.isEmpty()) {
                query = query + " AND data ->> 'country' = '" + sanitize(countryCode) + "' ";
            }

            query = query + " ORDER BY modified ASC LIMIT ? OFFSET ?";


            PreparedStatement statement = connection.prepareStatement(query);

            statement.setTimestamp(1, Timestamp.valueOf(timestamp));
            statement.setInt(2, PAGE_SIZE);
            statement.setInt(3, page * PAGE_SIZE);

            ResultSet rs = statement.executeQuery();

            List<T> result = new ArrayList<T>();

            while (rs.next()) {
                result.add(createFromResultSet(rs));
            }

            rs.close();
            statement.close();

            return result;
        } catch (Exception e) {
            logger.error("Unable to perform query, because of of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }

    @Override
    public final Integer getModifiedAfterCount(final LocalDateTime timestamp,
                                          final String createdBy,
                                          final String countryCode) {
        try {
            String query = "SELECT count(*) as total FROM " + getTableWithSchema() + " WHERE modified > ? ";

            if (createdBy!= null && !createdBy.isEmpty()) {
                query = query + " AND createdby = '" + sanitize(createdBy) + "' ";
            }

            if (countryCode!= null && !countryCode.isEmpty()) {
                query = query + " AND data ->> 'country' = '" + sanitize(countryCode) + "' ";
            }

            PreparedStatement statement = connection.prepareStatement(query);

            statement.setTimestamp(1, Timestamp.valueOf(timestamp));

            ResultSet rs = statement.executeQuery();

            Integer count = 0;

            while (rs.next()) {
                count = rs.getInt("total");
            }

            rs.close();
            statement.close();

            return count;
        } catch (Exception e) {
            logger.error("Unable to perform query, because of of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }

    /**
     * Creates item from result set.
     *
     * @param rs
     *         result set
     *
     * @return created item
     * @throws SQLException
     *         when retrieving of values fail
     */
    protected final T createFromResultSet(final ResultSet rs) throws SQLException {
        T t = getEmptyInstance();
        t.setId(rs.getString("id"));
        t.setModified(rs.getTimestamp("modified").toLocalDateTime());
        t.setModifiedBy(rs.getString("modifiedBy"));
        t.setModifiedByVersion(rs.getString("modifiedByVersion"));
        t.setCreated(rs.getTimestamp("created").toLocalDateTime());
        t.setCreatedBy(rs.getString("createdBy"));
        t.setCreatedByVersion(rs.getString("createdByVersion"));

        String data = rs.getString("data");
        try {
            mapper.readerForUpdating(t).readValue(data);
            logger.debug("Deserialized object {} with id {}", t, t.getId());
        } catch (IOException e) {
            logger.error("Unable to deserialize data from json exception {}", e);
            throw new UnrecoverableException("Unable to deserialize data from json", e);
        }
        t.setData(null);

        return t;
    }

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
                    .forEach(worker -> additionalWorkersRestriction.append(" OR (createdBy = '")
                            .append(worker.getKey())
                            .append("' AND createdByVersion = '")
                            .append(worker.getValue())
                            .append("')"));
            return additionalWorkersRestriction.toString();
        }
        return "";
    }

    /**
     * This method disables index scan. USE WITH EXTREME CARE! IMPROPER USE CAN
     * LEAD TO SIGNIFICANT PERFORMANCE PENALTIES OR EVEN FUNCTIONAL ISSUES! This
     * is handy for cases where native query planner failed in a creation of
     * proper execution plan.
     */
    protected final void disableIndexScan() {
        try {
            PreparedStatement st = connection.prepareStatement("SET enable_indexscan TO 'off';");

            st.executeUpdate();
        } catch (Exception e) {
            logger.error("Unable to perform query, because of of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }

    /**
     * This method enables index scan. USE WITH EXTREME CARE! IMPROPER USE CAN
     * LEAD TO SIGNIFICANT PERFORMANCE PENALTIES OR EVEN FUNCTIONAL ISSUES! This
     * is handy for cases where native query planner failed in a creation of
     * proper execution plan.
     */
    protected final void enableIndexScan() {
        try {
            PreparedStatement st = connection.prepareStatement("SET enable_indexscan TO 'on';");

            st.executeUpdate();
        } catch (Exception e) {
            logger.error("Unable to perform query, because of of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }

    /**
     * This method disables seq scan. USE WITH EXTREME CARE! IMPROPER USE CAN
     * LEAD TO SIGNIFICANT PERFORMANCE PENALTIES OR EVEN FUNCTIONAL ISSUES! This
     * is handy for cases where native query planner failed in a creation of
     * proper execution plan.
     */
    protected final void disableSeqScan() {
        try {
            PreparedStatement st = connection.prepareStatement("SET enable_seqscan TO 'off';");

            st.executeUpdate();
        } catch (Exception e) {
            logger.error("Unable to perform query, because of of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }

    /**
     * This method enables seq scan. USE WITH EXTREME CARE! IMPROPER USE CAN
     * LEAD TO SIGNIFICANT PERFORMANCE PENALTIES OR EVEN FUNCTIONAL ISSUES! This
     * is handy for cases where native query planner failed in a creation of
     * proper execution plan.
     */
    protected final void enableSeqScan() {
        try {
            PreparedStatement st = connection.prepareStatement("SET enable_seqscan TO 'on';");

            st.executeUpdate();
        } catch (Exception e) {
            logger.error("Unable to perform query, because of of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }

    /**
     * Returns empty instance.
     *
     * @return empty instance
     */
    public abstract T getEmptyInstance();

    /**
     * Returns table name including schema.
     *
     * @return table name including schema
     */
    protected abstract String getTableWithSchema();

    /**
     * Converts the object to json string.
     *
     * @param t
     *         object to be serialized
     *
     * @return serialized json string
     */
    private String serializeToJson(final T t) {
        try {
            // sanitize the input first
            t.setData(null);
            ObjectWriter objectWriter = mapper.writerWithDefaultPrettyPrinter();
            byte[] tWritten = objectWriter.writeValueAsBytes(t);

            // Written json
            String jsonData = new String(tWritten, "UTF-8");
            return jsonData;
        } catch (Exception e) {
            logger.error("Unable to serialize data to json exception {}", e);
            throw new UnrecoverableException("Unable to serialize data to json", e);
        }
    }

    @Override
    public final Boolean removeById(final String id) {
        try {
            PreparedStatement statement = connection.prepareStatement(
                    "DELETE FROM " + getTableWithSchema() + " WHERE id = ?");

            statement.setString(1, id);
            statement.executeUpdate();

            statement.close();

            return true;
        } catch (SQLException e) {
            logger.error("Unable to perform query, because of", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }

    @Override
    public final List<String> getIdsBySourceAndVersion(final String name, final String version) {
        try {
            PreparedStatement statement = connection.prepareStatement(
                    "SELECT id FROM " + getTableWithSchema() + ";");

            if (!name.isEmpty()) {
                statement = connection.prepareStatement(
                        "SELECT id FROM " + getTableWithSchema() + " WHERE createdby "
                                + "LIKE" + " ? AND createdbyversion LIKE ?");
                statement.setString(1, name);
                statement.setString(2, version);
            }

            ResultSet rs = statement.executeQuery();
            List<String> result = new ArrayList<String>();

            while (rs.next()) {
                result.add(rs.getString("id"));
            }

            rs.close();
            statement.close();

            return result;
        } catch (Exception e) {
            logger.error("Unable to perform query, because of of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }

    /**
     * Returns all tenders for the given buyer group id which have the first included publication of the given
     * {@code formType} published between {@code from} and {@code to} dates.
     *
     * @param buyerGroupId
     *      buyer group id
     * @param from
     *      from date
     * @param to
     *      to date
     * @param formType
     *      form type of included publication
     * @return list of buyer's tenders
     */
    public final List<T> getBuyerTendersInPeriod(final String buyerGroupId, final LocalDate from, final LocalDate to,
        final PublicationFormType formType) {

        try {
            PreparedStatement statement = connection.prepareStatement(
                "WITH rows AS ("
                    + " SELECT mt.*, min(p->>'publicationDate') AS first_published"
                    + " FROM " + getTableWithSchema() + " mt, jsonb_array_elements(mt.data#>'{publications}') p"
                    + " WHERE data @> '{\"buyers\": [{\"groupId\": \"" + buyerGroupId + "\"}]}'"
                    + " AND (p->>'isIncluded')::boolean"
                    + " AND p @> '{\"formType\":\"" + formType + "\"}'"
                    + " GROUP BY mt.id)"
                + " SELECT id, data, created, createdby, createdbyversion, modified, modifiedby, modifiedbyversion"
                + " FROM rows"
                + " WHERE first_published BETWEEN ? AND ?");

            statement.setString(1, from.toString());
            statement.setString(2, to.toString());

            ResultSet rs = statement.executeQuery();
            
            List<T> result = new ArrayList<>();
            while (rs.next()) {                
                result.add(createFromResultSet(rs));                
            }

            rs.close();
            statement.close();

            return result;
        } catch (Exception e) {
            logger.error("Unable to perform query, because of of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }

    /**
     * Returns median of the given CPV for tenders of the given type published in the given period.
     *
     * @param createdBy
     *      worker name
     * @param from
     *      start of the period
     * @param to
     *      end of the period
     * @param formType
     *      form type of included publication
     * @param cpv
     *      CPV for that the median to be calculated
     * @return array where index 0 holds median value for the given CPV and index 1 holds number of tenders used for median calculation
     */
    public final int[] getCPVMedianInPeriod(final List<String> createdBy, final LocalDate from, final LocalDate to,
        final PublicationFormType formType, final String cpv) {

        if (createdBy == null || createdBy.isEmpty() || cpv == null) {
            return new int[]{0, 0};
        }
        
        try {
            String createdByRestriction = String.join(" OR ", createdBy.stream().map(n -> "mt.createdBy = '" + n + "'")
                .collect(Collectors.toList()));

            PreparedStatement statement = connection.prepareStatement(
                // select suited tenders
                    "WITH"
                    + " tenders AS ("
                        + "	SELECT DISTINCT mt.*"
                        + "	FROM " + getTableWithSchema() + " mt, jsonb_array_elements(mt.data#>'{publications}') p,"
                        + " jsonb_array_elements(mt.data#>'{cpvs}') c"
                        + "	WHERE (" + createdByRestriction + ")"
                            + " AND mt.data->>'procedureType' <> 'NEGOTIATED_WITHOUT_PUBLICATION'"
                            + " AND (p->>'isIncluded')::boolean"
                            + " AND p@>'{\"formType\":\"" + formType + "\"}'"
                            + " AND p->>'publicationDate' BETWEEN ? AND ?"
                            + " AND (c->>'isMain')::boolean AND c->>'code' LIKE '" + cpv.replaceAll("\\-[0-9]$", "") + "%'"
                    + "),"
                    // select lot.bidsCount for each tender.lot and sort rows by count
                    + " bids AS ("
                        + " SELECT (l->>'bidsCount')::int AS bids_cnt, mt.id AS tender_id"
                        + " FROM tenders mt, jsonb_array_elements(mt.data#>'{lots}') l"
                        + "	WHERE l->>'bidsCount' IS NOT NULL"
                        + "	ORDER BY bids_cnt"
                    + "),"
                    // -- median calculation
                    + " counts AS ("
                        + "	SELECT array_agg(bids_cnt) AS bids_cnt, count(DISTINCT tender_id) AS tenders_cnt"
                        + "	FROM bids"
                    + ")"
                    + " SELECT CASE WHEN array_length(bids_cnt, 1) = 0 THEN 0"
                        + " WHEN array_length(bids_cnt, 1) % 2 = 0 THEN bids_cnt[array_length(bids_cnt, 1)/2]"
                        + " ELSE bids_cnt[(array_length(bids_cnt, 1)+1)/2] END AS median,"
                    + " tenders_cnt"
                    + " FROM counts");

            statement.setString(1, from.toString());
            statement.setString(2, to.toString());

            ResultSet rs = statement.executeQuery();
            int[] median;
            median = rs.next() ? new int[]{rs.getInt("median"), rs.getInt("tenders_cnt")} : new int[]{0, 0};

            rs.close();
            statement.close();

            return median;
        } catch (Exception e) {
            logger.error("Unable to perform query, because of of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }
}
