package eu.dl.dataaccess.dao.jdbc;

import com.fasterxml.jackson.databind.ObjectWriter;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.GenericDAO;
import eu.dl.dataaccess.dto.StorableDTO;

import java.io.IOException;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
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
     * @param pageSize page size
     * @return page with tenders
     */
    public final List<T> getByCountry(final String countryCode, final Integer page, final Integer pageSize) {
        try {
            PreparedStatement statement = getConnection().prepareStatement(
                    "SELECT * FROM " + getTableWithSchema() + " WHERE data ->> 'country' = '" +
                            sanitizeForJsonString(countryCode) + "' ORDER BY modified ASC LIMIT ? " +
                            "OFFSET ?");

            statement.setInt(1, pageSize);
            statement.setInt(2, page * pageSize);
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
     * Gets tenders for a specific country. Uses default page size.
     *
     * @param countryCode country code
     * @param page page
     * @return page with tenders
     */
    public final List<T> getByCountry(final String countryCode, final Integer page) {
        return getByCountry(countryCode, page, getPageSize());
    }

    /**
     * Gets tenders for a specific country and source.
     *
     * @param countryCode country code
     * @param page page
     * @param createdBy createdBy
     * @param pageSize page size
     * @return page with tenders
     */
    public final List<T> getByCountry(final String countryCode, final Integer page, final String createdBy, final Integer pageSize) {
        try {
            PreparedStatement statement = getConnection().prepareStatement("SELECT * FROM " + getTableWithSchema()
                + " WHERE data ->> 'country' = '" + sanitizeForJsonString(countryCode) + "' AND createdby = ?"
                + " ORDER BY modified ASC LIMIT ? OFFSET ?");

            statement.setString(1, createdBy);
            statement.setInt(2, pageSize);
            statement.setInt(3, page * pageSize);
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
     * Gets tenders for a specific country and source. Uses default page size.
     *
     * @param countryCode country code
     * @param page page
     * @param createdBy createdBy
     * @return page with tenders
     */
    public final List<T> getByCountry(final String countryCode, final Integer page, final String createdBy) {
        return getByCountry(countryCode, page, createdBy, getPageSize());
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
            PreparedStatement statement = getConnection().prepareStatement(
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

                PreparedStatement statement = getConnection().prepareStatement(
                        "SELECT * FROM " + getTableWithSchema() + " WHERE " + condition);

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
                    statement = getConnection().prepareStatement(
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
                    rs.close();
                } else {
                    // update
                    statement = getConnection().prepareStatement(
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

                statement.close();

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
            PreparedStatement statement = getConnection().prepareStatement(
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

            PreparedStatement statement = getConnection().prepareStatement(sql.toString());

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
                statement = getConnection().prepareStatement(
                        "SELECT id FROM " + getTableWithSchema() + " WHERE createdby = ? AND createdbyversion = ? "
                                + "AND modified >= ? AND modified <= ? ORDER BY data->>'processingOrder'");
                statement.setString(3, fromDate);
                statement.setString(4, toDate);
            } else if (fromDate != null) {
                statement = getConnection().prepareStatement(
                        "SELECT id FROM " + getTableWithSchema() + " WHERE createdby = ? AND createdbyversion = ? "
                                + "AND modified >= ? ORDER BY data->>'processingOrder'");
                statement.setString(3, fromDate);
            } else if (toDate != null) {
                statement = getConnection().prepareStatement(
                        "SELECT id FROM " + getTableWithSchema() + " WHERE createdby = ? AND createdbyversion = ? "
                                + "modified <= ? ORDER BY data->>'processingOrder'");
                statement.setString(3, toDate);
            } else {
                statement = getConnection().prepareStatement(
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
            PreparedStatement statement = getConnection().prepareStatement(
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
     * @param pageSize
     *      page size
     * @return list of objects with the same hash
     */
    public final List<T> getMine(final Integer page, final Integer pageSize) {
        try {
            PreparedStatement statement = getConnection().prepareStatement(
                    "SELECT * FROM " + getTableWithSchema()
                    + " WHERE ((createdBy = ? AND createdByVersion = ?) "
        				+ prepareAdditionalWorkersCondition() + ")"
                    + " ORDER BY data->>'processingOrder' ASC LIMIT ? OFFSET ?");

            statement.setString(1, workerName);
            statement.setString(2, workerVersion);
            statement.setInt(3, pageSize);
            statement.setInt(4, page * pageSize);

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
     * particular version of the matcher (or its relative). Uses default page size.
     *
     * @param page
     * 		   no. of page (from 0)
     * @return list of objects with the same hash
     */
    public final List<T> getMine(final Integer page) {
        return getMine(page, getPageSize());
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
            PreparedStatement statement = getConnection().prepareStatement(
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

    /**
     * Returns groupId of records with given hash which have been stored by specified workers (and versions of workers).
     *
     * @param hash hash to be searched
     * @return groupId of objects with the same hash and stored be one of the specified workers
     */
    public final String getGroupIdByHash(final String hash) {
        // prepare sql condition part for additional workers
        String additionalWorkersCondition = prepareAdditionalWorkersCondition();

        try {
            PreparedStatement statement = getConnection().prepareStatement(
                    "SELECT DISTINCT data->>'groupId' AS group_id FROM " + getTableWithSchema() + " WHERE data @> '{ " +
                            "\"hash\":\"" + sanitize(
                            hash) + "\"}' AND ( (createdBy = ? AND createdByVersion = ?) " +
                            additionalWorkersCondition + ")");

            statement.setString(1, workerName);
            statement.setString(2, workerVersion);

            ResultSet rs = statement.executeQuery();

            if (rs.next()) {
                String groupId = rs.getString("group_id");
                try {
                    if (rs.next()) {
                        logger.error("More than one unique groupId for hash {}", hash);
                        throw new UnrecoverableException("More than one unique groupId for hash.");
                    }
                    rs.close();
                    statement.close();
                    return groupId;
                } catch (Exception e) {
                    rs.close();
                    statement.close();
                    throw new UnrecoverableException("More than one unique groupId for hash.");
                }
            }
            rs.close();
            statement.close();
            return null;
        } catch (SQLException e) {
            logger.error("Unable to perform query, because of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        } catch (UnrecoverableException e) {
            logger.error("More than one unique groupId for hash {}", hash);
            throw new UnrecoverableException(e.getMessage(), e);
        }
    }

    @Override
    public final List<T> getModifiedAfter(final LocalDateTime timestamp, final Integer page, final Integer pageSize) {
        try {
            PreparedStatement statement = getConnection().prepareStatement(
                    "SELECT * FROM " + getTableWithSchema() + " WHERE modified > ? ORDER BY modified ASC LIMIT ? " +
                            "OFFSET ?");

            statement.setTimestamp(1, Timestamp.valueOf(timestamp));
            statement.setInt(2, pageSize);
            statement.setInt(3, page * pageSize);

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
    public final List<T> getModifiedAfter(final LocalDateTime timestamp, final Integer page) {
        return getModifiedAfter(timestamp, page, getPageSize());
    }

    @Override
    public final List<T> getModifiedAfter(final LocalDateTime timestamp, final String modifiedBy, final Integer page,
                                          final Integer pageSize) {
        try {
            PreparedStatement statement = getConnection().prepareStatement(
                    "SELECT * FROM " + getTableWithSchema() + " WHERE modified > ? AND createdby = ? ORDER BY " +
                            "modified ASC LIMIT ? OFFSET ?");

            statement.setTimestamp(1, Timestamp.valueOf(timestamp));
            statement.setString(2, modifiedBy);
            statement.setInt(3, pageSize);
            statement.setInt(4, page * pageSize);

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
        return getModifiedAfter(timestamp, modifiedBy, page, getPageSize());
    }

    @Override
    public final List<T> getModifiedAfter(final LocalDateTime timestamp, final String createdBy, final String countryCode,
                                          final Integer page, final Integer pageSize) {
        try {
            String query = "SELECT * FROM " + getTableWithSchema() + " WHERE modified > ? ";

            if (createdBy!= null && !createdBy.isEmpty()) {
                query = query + " AND createdby = '" + sanitize(createdBy) + "' ";
            }

            if (countryCode!= null && !countryCode.isEmpty()) {
                query = query + " AND data ->> 'country' = '" + sanitize(countryCode) + "' ";
            }

            query = query + " ORDER BY modified ASC LIMIT ? OFFSET ?";


            PreparedStatement statement = getConnection().prepareStatement(query);

            statement.setTimestamp(1, Timestamp.valueOf(timestamp));
            statement.setInt(2, pageSize);
            statement.setInt(3, page * pageSize);

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
    public final List<T> getModifiedAfter(final LocalDateTime timestamp, final String createdBy, final String countryCode,
                                          final Integer page) {
        return getModifiedAfter(timestamp, createdBy, countryCode, page, getPageSize());
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

            PreparedStatement statement = getConnection().prepareStatement(query);

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
            PreparedStatement st = getConnection().prepareStatement("SET enable_indexscan TO 'off';");

            st.executeUpdate();
            st.close();
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
            PreparedStatement st = getConnection().prepareStatement("SET enable_indexscan TO 'on';");

            st.executeUpdate();
            st.close();
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
            PreparedStatement st = getConnection().prepareStatement("SET enable_seqscan TO 'off';");

            st.executeUpdate();
            st.close();
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
            PreparedStatement st = getConnection().prepareStatement("SET enable_seqscan TO 'on';");

            st.executeUpdate();
            st.close();
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
            PreparedStatement statement = getConnection().prepareStatement(
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
            PreparedStatement statement = getConnection().prepareStatement(
                    "SELECT id FROM " + getTableWithSchema() + ";");

            if (!name.isEmpty()) {
                statement = getConnection().prepareStatement(
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
     * Returns last date of publication before {@code maxDate} for the given worker and version.
     *
     * @param createdBy
     *      worker name
     * @param createdByVersion
     *      worker version
     * @param maxDate
     *      maxDate
     * @return last publication date or null
     */
    public final LocalDate getLastPublicationDate(final String createdBy, final String createdByVersion, final LocalDate maxDate) {
        if (createdBy == null || createdByVersion == null) {
            return null;
        }

        try {
            PreparedStatement statement;
            if (maxDate != null) {
                statement = getConnection().prepareStatement(
                    "SELECT max(p->>'publicationDate') as last_date"
                        + " FROM " + getTableWithSchema() + " t, jsonb_array_elements(t.data->'publications') p"
                        + " WHERE createdby = ? AND createdbyversion = ? AND p->>'publicationDate' < ?");
                statement.setString(3, maxDate.format(DateTimeFormatter.ISO_LOCAL_DATE));
            } else {
                statement = getConnection().prepareStatement(
                    "SELECT max(p->>'publicationDate') as last_date"
                        + " FROM " + getTableWithSchema() + " t, jsonb_array_elements(t.data->'publications') p"
                        + " WHERE createdby = ? AND createdbyversion = ?");
            }

            statement.setString(1, createdBy);
            statement.setString(2, createdByVersion);

            ResultSet rs = statement.executeQuery();
            LocalDate lastDate = rs.next() ? rs.getDate("last_date").toLocalDate() : null;

            rs.close();
            statement.close();

            return lastDate;
        } catch (Exception e) {
            logger.error("Unable to perform query, because of of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }

    /**
     * Returns last date of publication for the given worker and version.
     *
     * @param createdBy
     *      worker name
     * @param createdByVersion
     *      worker version
     * @return last publication date or null
     */
    public final LocalDate getLastPublicationDate(final String createdBy, final String createdByVersion) {
        return getLastPublicationDate(createdBy, createdByVersion, null);
    }

    /**
     * Checks which hashes from the given list already exist in the database.
     *
     * @param hashes
     *      list of hashes to be looked for
     * @return list of existing hashes or empty list
     */
    public final List<String> existsHash(final List<String> hashes) {
        if (hashes == null || hashes.isEmpty()) {
            return Collections.EMPTY_LIST;
        }

        try {
            String restriction = hashes.stream()
                .filter(n -> n != null)
                .map(n -> String.format("'%s'", sanitize(n)))
                .collect(Collectors.joining(","));

            PreparedStatement statement = getConnection().prepareStatement(
                "SELECT data->>'hash' AS hash FROM " + getTableWithSchema() +
                " WHERE createdBy = ? AND createdByVersion = ? AND data->>'hash' IN (" + restriction + ")");

            statement.setString(1, workerName);
            statement.setString(2, workerVersion);

            ResultSet rs = statement.executeQuery();

            List<String> result = new ArrayList<>();
            while (rs.next()) {
                result.add(rs.getString("hash"));
            }

            rs.close();
            statement.close();

            return result;
        } catch (Exception e) {
            logger.error("Unable to perform query, because of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }
}
