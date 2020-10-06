package eu.datlab.dataaccess.dao.jdbc;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.MatchedTenderDAO;
import eu.dl.dataaccess.dao.jdbc.GenericJdbcDAO;
import eu.dl.dataaccess.dto.matched.MatchedTender;

import java.net.URL;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * JDBC implemenation of MatchedTenderDAO.
 */
public class JdbcMatchedTenderDAO extends GenericJdbcDAO<MatchedTender> implements MatchedTenderDAO<MatchedTender> {

    private static final String TABLE_NAME = "matched_tender";

    private static final int SELECT_DURATION_THRESHOLD = 50;

    @Override
    public final MatchedTender getEmptyInstance() {
        return new MatchedTender();
    }

    @Override
    protected final String getTableWithSchema() {
        return schema + "." + TABLE_NAME;
    }

    @Override
    public final List<MatchedTender> getByPublicationSourceIds(final List<String> publicationSourceIds) {
        if (publicationSourceIds == null || publicationSourceIds.isEmpty()) {
            return Collections.emptyList();
        }

        List<MatchedTender> result = new ArrayList<>();

        // Number of IDs can be ten, twenty or more than one hundred.
        // We know that big query is slow (more than 16 seconds) even if sequential scan and index scan are disabled,
        // because PostgreSQL still wants to use the scan for jsonb. But small query takes about 20 ms.
        // So we split query to many small queries and set execution plan (disable sequential scan and index scan) to
        // reduce query execution time.

        final int maxIdCountInOneQuery = 15;
        List<List<String>> queriesIds = new ArrayList<>();
        List<String> inputIds = publicationSourceIds;
        while (!inputIds.isEmpty()) {
            queriesIds.add(inputIds.subList(0,
                    inputIds.size() >= maxIdCountInOneQuery ? maxIdCountInOneQuery : inputIds.size()));
            inputIds = inputIds.subList(queriesIds.get(queriesIds.size() - 1).size(), inputIds.size());
        }

        final String additionalMatchersRestriction = prepareAdditionalWorkersCondition();

        for (List<String> queryIds : queriesIds) {
            assert queryIds.size() <= maxIdCountInOneQuery;
            StringBuilder idRestriction = new StringBuilder();
            for (String publicationSourceId : queryIds) {
                if (idRestriction.length() > 0) {
                    idRestriction.append(" or ");
                }
                idRestriction.append("data @> ")
                        .append("'{\"publications\":[{")
                        .append("\"sourceId\":\"")
                        .append(publicationSourceId)
                        .append("\"")
                        .append("}]}'");
            }

            try {
                PreparedStatement statement = connection.prepareStatement(
                        "SELECT * FROM " + getTableWithSchema() + " WHERE ((modifiedBy = ? AND modifiedByVersion = ?)"
                                + "" + " " + additionalMatchersRestriction + ") AND (" + idRestriction
                                .toString() + ")");

                statement.setString(1, workerName);
                statement.setString(2, workerVersion);

                disableSeqScan();
                disableIndexScan();

                long selectStartTime = System.currentTimeMillis();
                ResultSet rs = statement.executeQuery();
                long selectEndTime = System.currentTimeMillis();
                logger.info("Selection of matched tenders by publicationsSourceIdIntersection {} took {} ms.",
                        this.getClass().getName(), selectEndTime - selectStartTime);
                if (selectEndTime - selectStartTime > SELECT_DURATION_THRESHOLD) {
                    logger.warn("Too long selection of matched tenders {} ms. Query {} ",
                            selectEndTime - selectStartTime, rs.getStatement().toString());
                }

                enableSeqScan();
                enableIndexScan();

                while (rs.next()) {
                    MatchedTender matchedTender = createFromResultSet(rs);
                    if (!result.stream().anyMatch(t -> t.getId().equals(matchedTender.getId()))) {
                        result.add(matchedTender);
                    }
                }

                rs.close();
                statement.close();
            } catch (Exception e) {
                logger.error("Unable to perform query, because of of {}", e);
                throw new UnrecoverableException("Unable to perform query.", e);
            }
        }

        return result;
    }

    @Override
    public final List<MatchedTender> getByPublicationHumanReadableUrls(final List<URL> publicationHumanReadableUrls) {
        if (publicationHumanReadableUrls == null || publicationHumanReadableUrls.isEmpty()) {
            return Collections.emptyList();
        }

        List<MatchedTender> result = new ArrayList<>();

        // Number of URLs can be ten, twenty or more than one hundred.
        // We know that big query is slow (more than 16 seconds) even if sequential scan and index scan are disabled,
        // because PostgreSQL still wants to use the scan for jsonb. But small query takes about 20 ms.
        // So we split query to many small queries and set execution plan (disable sequential scan and index scan) to
        // reduce query execution time.

        final int maxUrlCountInOneQuery = 15;
        List<List<URL>> queriesUrls = new ArrayList<>();
        List<URL> inputUrls = publicationHumanReadableUrls;
        while (!inputUrls.isEmpty()) {
            queriesUrls.add(inputUrls.subList(0,
                    inputUrls.size() >= maxUrlCountInOneQuery ? maxUrlCountInOneQuery : inputUrls.size()));
            inputUrls = inputUrls.subList(queriesUrls.get(queriesUrls.size() - 1).size(), inputUrls.size());
        }

        final String additionalMatchersRestriction = prepareAdditionalWorkersCondition();

        for (List<URL> queryUrls : queriesUrls) {
            assert queryUrls.size() <= maxUrlCountInOneQuery;
            StringBuilder urlRestriction = new StringBuilder();
            for (URL url : queryUrls) {
                if (urlRestriction.length() > 0) {
                    urlRestriction.append(" or ");
                }
                urlRestriction.append("data @> ")
                        .append("'{\"publications\":[{")
                        .append("\"humanReadableUrl\":\"")
                        .append(url)
                        .append("\"")
                        .append("}]}'");
            }

            try {
                PreparedStatement statement = connection.prepareStatement(
                        "SELECT * FROM " + getTableWithSchema() + " WHERE ((modifiedBy = ? AND modifiedByVersion = ?)"
                                + "" + " " + additionalMatchersRestriction + ") AND (" + urlRestriction
                                .toString() + ")");

                statement.setString(1, workerName);
                statement.setString(2, workerVersion);

                disableSeqScan();
                disableIndexScan();

                ResultSet rs = statement.executeQuery();

                enableSeqScan();
                enableIndexScan();

                while (rs.next()) {
                    MatchedTender matchedTender = createFromResultSet(rs);
                    if (!result.stream().anyMatch(t -> t.getId().equals(matchedTender.getId()))) {
                        result.add(matchedTender);
                    }
                }

                rs.close();
                statement.close();
            } catch (Exception e) {
                logger.error("Unable to perform query, because of of {}", e);
                throw new UnrecoverableException("Unable to perform query.", e);
            }
        }

        return result;
    }

    @Override
    public final List<MatchedTender> getByPublicationMachineReadableUrls(
            final List<URL> publicationMachineReadableUrls) {
        if (publicationMachineReadableUrls == null || publicationMachineReadableUrls.isEmpty()) {
            return Collections.emptyList();
        }

        List<MatchedTender> result = new ArrayList<>();

        // Number of URLs can be ten, twenty or more than one hundred.
        // We know that big query is slow (more than 16 seconds) even if sequential scan and index scan are disabled,
        // because PostgreSQL still wants to use the scan for jsonb. But small query takes about 20 ms.
        // So we split query to many small queries and set execution plan (disable sequential scan and index scan) to
        // reduce query execution time.

        final int maxUrlCountInOneQuery = 15;
        List<List<URL>> queriesUrls = new ArrayList<>();
        List<URL> inputUrls = publicationMachineReadableUrls;
        while (!inputUrls.isEmpty()) {
            queriesUrls.add(inputUrls.subList(0,
                    inputUrls.size() >= maxUrlCountInOneQuery ? maxUrlCountInOneQuery : inputUrls.size()));
            inputUrls = inputUrls.subList(queriesUrls.get(queriesUrls.size() - 1).size(), inputUrls.size());
        }

        final String additionalMatchersRestriction = prepareAdditionalWorkersCondition();

        for (List<URL> queryUrls : queriesUrls) {
            assert queryUrls.size() <= maxUrlCountInOneQuery;
            StringBuilder urlRestriction = new StringBuilder();
            for (URL url : queryUrls) {
                if (urlRestriction.length() > 0) {
                    urlRestriction.append(" or ");
                }
                urlRestriction.append("data @> ")
                        .append("'{\"publications\":[{")
                        .append("\"machineReadableUrl\":\"")
                        .append(url)
                        .append("\"")
                        .append("}]}'");
            }

            try {
                PreparedStatement statement = connection.prepareStatement(
                        "SELECT * FROM " + getTableWithSchema() + " WHERE ((modifiedBy = ? AND modifiedByVersion = ?)"
                                + "" + " " + additionalMatchersRestriction + ") AND (" + urlRestriction
                                .toString() + ")");

                statement.setString(1, workerName);
                statement.setString(2, workerVersion);

                disableSeqScan();
                disableIndexScan();

                ResultSet rs = statement.executeQuery();

                enableSeqScan();
                enableIndexScan();

                while (rs.next()) {
                    MatchedTender matchedTender = createFromResultSet(rs);
                    if (!result.stream().anyMatch(t -> t.getId().equals(matchedTender.getId()))) {
                        result.add(matchedTender);
                    }
                }

                rs.close();
                statement.close();
            } catch (Exception e) {
                logger.error("Unable to perform query, because of of {}", e);
                throw new UnrecoverableException("Unable to perform query.", e);
            }
        }

        return result;
    }

    @Override
    public final List<MatchedTender> getByDocumentsUrl(final URL documentsUrl) {
        if (documentsUrl == null) {
            return Collections.emptyList();
        }

        List<MatchedTender> result = new ArrayList<>();

        final String additionalMatchersRestriction = prepareAdditionalWorkersCondition();

        StringBuilder urlRestriction = new StringBuilder();
        urlRestriction
                .append("data @> ")
                .append("'{\"documentsLocation\":{")
                .append("\"url\":\"")
                .append(documentsUrl.toString())
                .append("\"")
                .append("}}'");

        try {
            PreparedStatement statement = connection.prepareStatement(
                    "SELECT * FROM " + getTableWithSchema() + " WHERE ((modifiedBy = ? AND modifiedByVersion = ?) "
                            + additionalMatchersRestriction + ") AND (" + urlRestriction.toString() + ")");

            statement.setString(1, workerName);
            statement.setString(2, workerVersion);

            ResultSet rs = statement.executeQuery();

            while (rs.next()) {
                MatchedTender matchedTender = createFromResultSet(rs);
                if (!result.stream().anyMatch(t -> t.getId().equals(matchedTender.getId()))) {
                    result.add(matchedTender);
                }
            }

            rs.close();
            statement.close();
        } catch (Exception e) {
            logger.error("Unable to perform query, because of of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }

        return result;
    }

    @Override
    public final List<MatchedTender> getForResend(final String name, final String version) {
        try {
            PreparedStatement statement = connection.prepareStatement(
                    "SELECT DISTINCT data->>'groupId' AS groupId FROM " + getTableWithSchema() + " WHERE modifiedby "
                            + "=" + " ? AND modifiedbyversion = ?");

            statement.setString(1, name);
            statement.setString(2, version);

            ResultSet rs = statement.executeQuery();
            List<MatchedTender> result = new ArrayList<MatchedTender>();

            while (rs.next()) {
                MatchedTender mt = new MatchedTender();
                mt.setGroupId(rs.getString("groupId"));
                result.add(mt);
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
    public final List<MatchedTender> getByPublicationSourceIdsAndPublicationDates(
        final Map<String, LocalDate> sourceIdsAndDates) {

        if (sourceIdsAndDates == null || sourceIdsAndDates.isEmpty()) {
            return Collections.emptyList();
        }

        List<MatchedTender> result = new ArrayList<>();

        // Number of IDs can be ten, twenty or more than one hundred.
        // We know that big query is slow (more than 16 seconds) even if sequential scan and index scan are disabled,
        // because PostgreSQL still wants to use the scan for jsonb. But small query takes about 20 ms.
        // So we split query to many small queries and set execution plan (disable sequential scan and index scan) to
        // reduce query execution time.

        final int maxIdCountInOneQuery = 15;

        List<Map<String, LocalDate>> queries = new ArrayList<>();
        Map<String, LocalDate> subMap = null;
        for (Map.Entry<String, LocalDate> n : sourceIdsAndDates.entrySet()) {
            if (subMap == null || subMap.size() == maxIdCountInOneQuery) {
                queries.add(new HashMap<>());
                subMap = queries.get(queries.size() - 1);
            }
            
            subMap.put(n.getKey(), n.getValue());
        }
        
        final String additionalMatchersRestriction = prepareAdditionalWorkersCondition();

        for (Map<String, LocalDate> query : queries) {
            assert query.size() <= maxIdCountInOneQuery;

            String restriction = query.entrySet().stream()
                .map(n -> {
                    return "data @> '{\"publications\":[{"
                        + "\"sourceId\":\"" + n.getKey() + "\","
                        + "\"publicationDate\":\"" + n.getValue() + "\"}]}'";
                })
                .collect(Collectors.joining(" OR "));
            
            try {
                PreparedStatement statement = connection.prepareStatement(
                        "SELECT * FROM " + getTableWithSchema() + " WHERE ((modifiedBy = ? AND modifiedByVersion = ?)"
                                + "" + " " + additionalMatchersRestriction + ") AND (" + restriction + ")");

                statement.setString(1, workerName);
                statement.setString(2, workerVersion);

                disableSeqScan();
                disableIndexScan();

                long selectStartTime = System.currentTimeMillis();
                ResultSet rs = statement.executeQuery();
                long selectEndTime = System.currentTimeMillis();
                logger.info("Selection of matched tenders by publicationsSourceIdIntersection {} took {} ms.",
                        this.getClass().getName(), selectEndTime - selectStartTime);
                if (selectEndTime - selectStartTime > SELECT_DURATION_THRESHOLD) {
                    logger.warn("Too long selection of matched tenders {} ms. Query {} ",
                            selectEndTime - selectStartTime, rs.getStatement().toString());
                }

                enableSeqScan();
                enableIndexScan();

                while (rs.next()) {
                    MatchedTender matchedTender = createFromResultSet(rs);
                    if (!result.stream().anyMatch(t -> t.getId().equals(matchedTender.getId()))) {
                        result.add(matchedTender);
                    }
                }

                rs.close();
                statement.close();
            } catch (Exception e) {
                logger.error("Unable to perform query, because of of {}", e);
                throw new UnrecoverableException("Unable to perform query.", e);
            }
        }

        return result;
    }


}
