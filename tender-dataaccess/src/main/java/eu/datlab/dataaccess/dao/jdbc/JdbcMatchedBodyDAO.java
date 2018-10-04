package eu.datlab.dataaccess.dao.jdbc;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.MatchedBodyDAO;
import eu.dl.dataaccess.dao.jdbc.GenericJdbcDAO;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.codetables.BodyType;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.matched.MatchedGroupInfo;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * JDBC implementation of master tender DAO.
 */
public class JdbcMatchedBodyDAO extends GenericJdbcDAO<MatchedBody> implements MatchedBodyDAO<MatchedBody> {

    private static final String TABLE_NAME = "matched_body";

    private static final int SELECT_DURATION_THRESHOLD = 50;

    @Override
    public final MatchedBody getEmptyInstance() {
        return new MatchedBody();
    }

    @Override
    protected final String getTableWithSchema() {
        return schema + "." + TABLE_NAME;
    }

    @Override
    public final List<MatchedBody> getExactMatchBodiesPool(final String standardizedName,
                                                           final String standardizedAddress, final
                                                           List<BodyIdentifier> bodyIds) {

        StringBuilder restriction = exactMatchRestrictionBuilder(standardizedName, standardizedAddress, bodyIds);
        String additionalMatchersRestriction = prepareAdditionalWorkersCondition();

        if (restriction.length() == 0) {
            return Collections.emptyList();
        }
        try {
            PreparedStatement statement = connection.prepareStatement(
                    "SELECT * FROM " + getTableWithSchema() + " WHERE ((modifiedBy = ? AND modifiedByVersion = ?) " +
                            additionalMatchersRestriction + ") AND (" + restriction
                            .toString() + ")");

            statement.setString(1, workerName);
            statement.setString(2, workerVersion);

            ResultSet rs = statement.executeQuery();

            List<MatchedBody> result = new ArrayList<MatchedBody>();
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
    public final List<MatchedBody> getApproximateMatchBodiesPool(final String standardizedName,
                                                                 final String standardizedAddress, final
                                                                 List<BodyIdentifier> bodyIds, final String
                                                                         digest) {

        StringBuilder restriction = approximateMatchRestrictionBuilder(standardizedName, standardizedAddress, bodyIds,
                digest);
        String additionalMatchersRestriction = prepareAdditionalWorkersCondition();

        if (restriction.length() == 0) {
            return Collections.emptyList();
        }

        try {

            PreparedStatement statement = connection.prepareStatement(
                    "SELECT * FROM " + getTableWithSchema() + " WHERE ((modifiedBy = ? AND modifiedByVersion = ?) " +
                            additionalMatchersRestriction + ") AND (" + restriction
                            .toString() + ")");

            statement.setString(1, workerName);
            statement.setString(2, workerVersion);

            long selectStartTime = System.currentTimeMillis();
            ResultSet rs = statement.executeQuery();
            long selectEndTime = System.currentTimeMillis();
            if (selectEndTime - selectStartTime > SELECT_DURATION_THRESHOLD) {
                logger.warn("Too long selection of matched bodies for approximate matching {} ms. Query {} ",
                        selectEndTime - selectStartTime, rs.getStatement().toString());
            }

            List<MatchedBody> result = new ArrayList<MatchedBody>();
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
    public final MatchedBody getByEtalonId(final String id) {
        if (id == null) {
            return null;
        }

        StringBuilder bodyIdsRestriction = new StringBuilder();
        bodyIdsRestriction.append("data @> ")
                .append("'{\"bodyIds\":[{")
                .append("\"id\":\"")
                .append(sanitizeForJsonString(id))
                .append("\",")
                .append("\"scope\":\"")
                .append(BodyIdentifier.Scope.ETALON_ID)
                .append("\"")
                .append("}]}'");

        String additionalMatchersRestriction = prepareAdditionalWorkersCondition();

        try {
            PreparedStatement statement = connection.prepareStatement(
                    "SELECT * FROM " + getTableWithSchema() + " WHERE ((modifiedBy = ? AND modifiedByVersion = ?) " +
                            additionalMatchersRestriction + ") AND " + bodyIdsRestriction
                            .toString());

            statement.setString(1, workerName);
            statement.setString(2, workerVersion);

            ResultSet rs = statement.executeQuery();

            MatchedBody result = null;

            if (rs.next()) {
                result = createFromResultSet(rs);
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
    public final List<MatchedBody> getForResend(final String name, final String version) {
        try {
            PreparedStatement statement = connection.prepareStatement(
                    "SELECT DISTINCT data->>'groupId' AS groupId FROM " + getTableWithSchema() + " WHERE modifiedby "
                            + "=" + " ? AND modifiedbyversion = ?");

            statement.setString(1, name);
            statement.setString(2, version);

            ResultSet rs = statement.executeQuery();
            List<MatchedBody> result = new ArrayList<MatchedBody>();

            while (rs.next()) {
                MatchedBody mb = new MatchedBody();
                mb.setGroupId(rs.getString("groupId"));
                result.add(mb);
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
     * Returns restriction for matched body database query that returns etalons
     * for exact matching. Null values of input parameters aren't taken into
     * account.
     *
     * @param standardizedName    standardized name
     * @param standardizedAddress standardized address
     * @param bodyIds             list of body identifiers
     *
     * @return restriction as StringBuilder instance
     */
    private StringBuilder exactMatchRestrictionBuilder(final String standardizedName, final String standardizedAddress,
                                                       final List<BodyIdentifier> bodyIds) {

        StringBuilder restriction = new StringBuilder();
        if (standardizedName != null) {
            restriction.append(" md5(data ->>'standardizedName') = md5('")
                    .append(sanitizeForJsonString(standardizedName))
                    .append("')");
        }
        if (standardizedAddress != null) {
            restriction.append(restriction.length() > 0 ? " OR " : "");
            restriction.append(" md5(data ->>'standardizedAddress') = md5('")
                    .append(sanitizeForJsonString(standardizedAddress))
                    .append("')");
        }
        if (bodyIds != null) {
            StringBuilder bodyIdsRestriction = new StringBuilder();
            bodyIds.stream().forEach((id) -> {
                if (id.getId() != null && id.getScope() != null) {
                    bodyIdsRestriction.append(bodyIdsRestriction.length() > 0 ? " OR " : "")
                            .append("data @> '{\"bodyIds\": ")
                            .append("[{")
                            .append("\"id\":\"")
                            .append(sanitizeForJsonString(id.getId()))
                            .append("\",")
                            .append("\"scope\":\"")
                            .append(id.getScope())
                            .append("\"")
                            .append("}]}'");
                }
            });

            if (bodyIdsRestriction.length() > 0) {
                restriction.append(restriction.length() > 0 ? " OR " : "").append(bodyIdsRestriction);
            }
        }

        return restriction;
    }

    /**
     * Returns restriction for matched body database query that returns etalons
     * for approximate matching. Null values of input parameters aren't taken
     * into account.
     *
     * @param standardizedName    standardized name
     * @param standardizedAddress standardized address
     * @param bodyIds             list of body identifiers
     * @param digest              digest
     *
     * @return restriction as StringBuilder instance
     */
    private StringBuilder approximateMatchRestrictionBuilder(final String standardizedName,
                                                             final String standardizedAddress, final
                                                             List<BodyIdentifier> bodyIds, final String digest) {

        StringBuilder restriction = exactMatchRestrictionBuilder(standardizedName, standardizedAddress, bodyIds);

        if (digest != null) {
            restriction.append(restriction.length() > 0 ? " OR " : "");
            restriction.append(" data ->>'digest' = '").append(sanitizeForJsonString(digest))
                    .append("'");
        }

        return restriction;
    }

    @Override
    public final List<MatchedGroupInfo> getGroupsInfo(final List<String> groups) {
        if (groups == null || groups.isEmpty()) {
            return Collections.emptyList();
        }

        try {
            final StringBuilder gidStm = new StringBuilder();
            groups.forEach((gid) -> {
                if (gidStm.length() > 0) {
                    gidStm.append(" or ");
                }
                gidStm.append("data @> '{\"groupId\":\"").append(sanitizeForJsonString(gid)).append("\"}'");
            });

            PreparedStatement statement = connection.prepareStatement("select data#>>'{groupId}' as gid,"
                    + "SUM((data @> '{\"bodyIds\": [{\"type\": \"ETALON_ID\"}]}')::int) > 0 as hasEtalon,"
                    + "COUNT(*) as size"
                    + " FROM " + getTableWithSchema()
                    + " WHERE modifiedby = '" + workerName + "' AND modifiedbyversion = '" + workerVersion + "'"
                    + " AND (" + gidStm.toString() + ")"
                    + " GROUP BY gid ORDER BY size DESC");

            ResultSet rs = statement.executeQuery();
            List<MatchedGroupInfo> result = new ArrayList<>();

            while (rs.next()) {
                result.add(new MatchedGroupInfo()
                        .setGroupId(rs.getString("gid"))
                        .setSize(rs.getInt("size"))
                        .setHasEtalon(rs.getBoolean("hasEtalon")));
            }

            rs.close();
            statement.close();

            return result;
        } catch (SQLException e) {
            logger.error("Unable to perform query, because of of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }

    @Override
    public final List<String> getEtalonGroupIds() {
        ArrayList<String> result = new ArrayList<String>();

        StringBuilder bodyIdsRestriction = new StringBuilder();
        bodyIdsRestriction.append("data @> ")
                .append("'{\"bodyIds\":[{")
                .append("\"scope\":\"")
                .append(BodyIdentifier.Scope.ETALON_ID)
                .append("\"")
                .append("}]}'");

        String additionalMatchersRestriction = prepareAdditionalWorkersCondition();

        try {
            PreparedStatement statement = connection.prepareStatement(
                    "SELECT data->'groupId' as groupId FROM " + getTableWithSchema()
                            + " WHERE ((createdBy = ? AND createdByVersion = ?) "
                            + additionalMatchersRestriction + ") AND " + bodyIdsRestriction
                            .toString());

            statement.setString(1, workerName);
            statement.setString(2, workerVersion);

            ResultSet rs = statement.executeQuery();

            while (rs.next()) {
                result.add(rs.getString("groupId"));
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
    public final Map<String, String> getHashAndGroupIds() {
        final Map<String, String> result = new HashMap<>();

        String oneBeforePreviousDate = null;
        String previousDate = null;

        try {
            while (true) {
                PreparedStatement statement = connection.prepareStatement(
                    "SELECT modified, data->>'groupId' as groupId, data->>'hash' as hash,"
                        + " jsonb_array_elements(data->'alternativeHashes')->>'hash' as alternativeHash"
                    + " FROM " + getTableWithSchema()
                    + " WHERE ((createdBy = ? AND createdByVersion = ?)" + prepareAdditionalWorkersCondition() + ")"
                        + prepareModifiedAfterCondition(previousDate)
                    + " ORDER BY modified ASC LIMIT 100000;");
                
                statement.setString(1, workerName);
                statement.setString(2, workerVersion);

                ResultSet rs = statement.executeQuery();

                boolean noResults = true;

                while (rs.next()) {
                    final String groupId = rs.getString("groupId");

                    if (groupId != null && !groupId.isEmpty()) {
                        final String hash = rs.getString("hash");
                        final String alternativeHash = rs.getString("alternativeHash");

                        if (hash != null && !hash.isEmpty()) {
                            result.put(hash, groupId);
                        }

                        if (alternativeHash != null && !alternativeHash.isEmpty()) {
                            result.put(alternativeHash, groupId);
                        }
                    }

                    previousDate = rs.getString("modified");
                    noResults = false;
                }

                rs.close();
                statement.close();

                if (noResults) {
                    break;
                }

                if (oneBeforePreviousDate != null && oneBeforePreviousDate.equals(
                        previousDate)) {
                    break;
                } else {
                    oneBeforePreviousDate = previousDate;
                }
            }

            return result;
        } catch (Exception e) {
            logger.error("Unable to perform query, because of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }

    /**
     * Create sql condition if lastDate is not null.
     *
     * @param lastDate date by which to filter
     * @return String or null
     */
    private String prepareModifiedAfterCondition(final String lastDate) {
        return lastDate == null ? "" : " AND (modified > '" + lastDate + "')";
    }

    @Override
    public final List<MatchedBody> getByNameAndRole(final String name, final BodyType role) {
        if (name == null || role == null) {
            return Collections.emptyList();
        }

        try {
            PreparedStatement statement = connection.prepareStatement(
                "SELECT * FROM " + getTableWithSchema()
                    + " WHERE ((createdBy = ? AND createdByVersion = ?) " + prepareAdditionalWorkersCondition() + ")" +
                    " AND data @> '{\"name\":\"" + name +"\", \"role\":\"" + role + "\"}'");

            statement.setString(1, workerName);
            statement.setString(2, workerVersion);

            ResultSet rs = statement.executeQuery();

            List<MatchedBody> result = new ArrayList<MatchedBody>();
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
}
