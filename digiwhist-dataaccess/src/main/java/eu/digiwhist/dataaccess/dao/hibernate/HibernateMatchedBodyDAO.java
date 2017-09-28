package eu.digiwhist.dataaccess.dao.hibernate;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import javax.persistence.Query;

import eu.dl.dataaccess.dao.MatchedBodyDAO;
import eu.dl.dataaccess.dao.hibernate.GenericHibernateDAO;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.matched.MatchedBody;
import eu.dl.dataaccess.dto.matched.MatchedGroupInfo;

/**
 * Hibernate DAO implementation for tenders.
 */
public class HibernateMatchedBodyDAO extends GenericHibernateDAO<MatchedBody> implements MatchedBodyDAO<MatchedBody> {

    @Override
    protected final Class<MatchedBody> getDTOClass() {
        return MatchedBody.class;
    }

    @Override
    public final MatchedBody getEmptyInstance() {
        return new MatchedBody();
    }

    @Override
    public final List<MatchedBody> getByGroupId(final String groupId) {
        Query q = entityManager.createQuery("SELECT e FROM " + getDTOClass().getName() + " e WHERE groupId = :groupId");
        q.setParameter("groupId", groupId);

        List<MatchedBody> result = (List<MatchedBody>) q.getResultList();

        return deserialize(result);
    }

    @Override
    public final List<MatchedBody> getMineByHash(final String hash) {
        Query q = entityManager.createQuery(
                "SELECT e FROM " + getDTOClass().getName() + " e WHERE hash = :hash AND modifiedBy = :modifiedBy AND "
                        + "" + "modifiedByVersion = :modifiedByVersion) ");

        q.setParameter("modifiedByVersion", workerVersion);
        q.setParameter("modifiedBy", workerName);
        q.setParameter("hash", hash);

        List<MatchedBody> result = (List<MatchedBody>) q.getResultList();

        return deserialize(result);
    }

    @Override
    public final List<MatchedBody> getByHash(final String hash) {
        String additionalMatchersRestriction = prepareAdditionalWorkersCondition();

        Query q = entityManager.createQuery(
                "SELECT e FROM " + getDTOClass().getName() + " e WHERE hash = :hash AND ( (modifiedBy = :modifiedBy "
                        + "AND modifiedByVersion = :modifiedByVersion) " + additionalMatchersRestriction + ")");

        q.setParameter("modifiedByVersion", workerVersion);
        q.setParameter("modifiedBy", workerName);
        q.setParameter("hash", hash);

        List<MatchedBody> result = (List<MatchedBody>) q.getResultList();

        return deserialize(result);
    }

    @Override
    public final List<MatchedBody> getExactMatchBodiesPool(final String standardizedName,
            final String standardizedAddress, final List<BodyIdentifier> bodyIds) {

        StringBuilder restriction = exactMatchRestrictionBuilder(standardizedName, standardizedAddress, bodyIds);
        String additionalMatchersRestriction = prepareAdditionalWorkersCondition();

        if (restriction.length() == 0) {
            return Collections.emptyList();
        }

        Query q = entityManager.createNativeQuery(
                "SELECT * FROM {h-schema}matched_body" + " WHERE ( (modifiedBy = :modifiedBy " + "AND " +
                        "modifiedByVersion = :modifiedByVersion) " + additionalMatchersRestriction + " AND (" +
                        restriction
                        .toString() + ")", MatchedBody.class);

        q.setParameter("modifiedBy", workerName);
        q.setParameter("modifiedByVersion", workerVersion);

        return (List<MatchedBody>) deserialize(q.getResultList());
    }

    @Override
    public final List<MatchedBody> getApproximateMatchBodiesPool(final String standardizedName,
            final String standardizedAddress, final List<BodyIdentifier> bodyIds, final String digest) {

        StringBuilder restriction = approximateMatchRestrictionBuilder(standardizedName, standardizedAddress, bodyIds,
                digest);
        String additionalMatchersRestriction = prepareAdditionalWorkersCondition();

        if (restriction.length() == 0) {
            return Collections.emptyList();
        }

        Query q = entityManager.createNativeQuery(
                "SELECT * FROM {h-schema}matched_body" + " WHERE ( (modifiedBy = :modifiedBy " + "AND " +
                        "modifiedByVersion = :modifiedByVersion) " + additionalMatchersRestriction + " AND (" +
                        restriction
                        .toString() + ")", MatchedBody.class);

        q.setParameter("modifiedBy", workerName);
        q.setParameter("modifiedByVersion", workerVersion);

        return (List<MatchedBody>) deserialize(q.getResultList());
    }

    /**
     * Returns restriction for matched body database query that returns etalons for exact matching. Null values of input
     * parameters aren't taken into account.
     *
     * @param standardizedName
     *         standardized name
     * @param standardizedAddress
     *         standardized address
     * @param bodyIds
     *         list of body identifiers
     *
     * @return restriction as StringBuilder instance
     */
    private StringBuilder exactMatchRestrictionBuilder(final String standardizedName, final String standardizedAddress,
            final List<BodyIdentifier> bodyIds) {

        StringBuilder restriction = new StringBuilder();
        if (standardizedName != null) {
            restriction.append("standardizedname = '").append(sanitize(standardizedName)).append("'");
        }
        if (standardizedAddress != null) {
            restriction.append(restriction.length() > 0 ? " OR " : "")
                    .append("standardizedaddress = '")
                    .append(sanitize(standardizedAddress))
                    .append("'");
        }
        if (bodyIds != null) {
            StringBuilder bodyIdsRestriction = new StringBuilder();
            bodyIds.stream().forEach((id) -> {
                if (id.getId() != null && id.getScope() != null) {
                    bodyIdsRestriction.append(bodyIdsRestriction.length() > 0 ? " OR " : "")
                            .append("data @> '{\"bodyIds\": ")
                            .append("[{")
                            .append("\"id\":\"")
                            .append(sanitize(id.getId()))
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
     * Returns restriction for matched body database query that returns etalons for approximate matching. Null values of
     * input parameters aren't taken into account.
     *
     * @param standardizedName
     *         standardized name
     * @param standardizedAddress
     *         standardized address
     * @param bodyIds
     *         list of body identifiers
     * @param digest
     *         digest
     *
     * @return restriction as StringBuilder instance
     */
    private StringBuilder approximateMatchRestrictionBuilder(final String standardizedName,
            final String standardizedAddress, final List<BodyIdentifier> bodyIds, final String digest) {

        StringBuilder restriction = exactMatchRestrictionBuilder(standardizedName, standardizedAddress, bodyIds);

        if (digest != null) {
            restriction.append(restriction.length() > 0 ? " OR " : "")
                    .append("digest = '")
                    .append(sanitize(digest))
                    .append("'");
        }

        return restriction;
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
                .append(id)
                .append("\",")
                .append("\"scope\":\"")
                .append(BodyIdentifier.Scope.ETALON_ID)
                .append("\"")
                .append("}]}'");

        String additionalMatchersRestriction = prepareAdditionalWorkersCondition();

        Query q = entityManager.createNativeQuery(
                "SELECT * FROM {h-schema}matched_body" + " WHERE ( (modifiedBy = :modifiedBy " + "AND " +
                        "modifiedByVersion = :modifiedByVersion) " + additionalMatchersRestriction + " AND " +
                        bodyIdsRestriction
                        .toString(), MatchedBody.class);

        q.setParameter("modifiedByVersion", workerVersion);
        q.setParameter("modifiedBy", workerName);

        return deserialize((MatchedBody) q.getSingleResult());
    }

    @Override
    public final List<MatchedBody> getForResend(final String workerName, final String workerVersion) {
        Query countQuery = entityManager.createQuery(
                "SELECT count(*) FROM " + getDTOClass().getName() + " e WHERE modifiedBy = :modifiedBy AND " +
                        "modifiedByVersion = :modifiedByVersion");
        countQuery.setParameter("modifiedByVersion", workerVersion);
        countQuery.setParameter("modifiedBy", workerName);
        Long count = ((Long) countQuery.getSingleResult());
        Integer page = 0;
        List<MatchedBody> result = new ArrayList<>();

        while ((page * PAGE_SIZE) < count) {
            Query q = entityManager.createQuery(
                    "SELECT e FROM " + getDTOClass().getName() + " e WHERE modifiedBy = :modifiedBy AND " +
                            "modifiedByVersion = :modifiedByVersion");
            q.setParameter("modifiedByVersion", workerVersion);
            q.setParameter("modifiedBy", workerName);
            q.setFirstResult(page * PAGE_SIZE);
            q.setMaxResults(PAGE_SIZE);

            List<MatchedBody> pagedResult = (List<MatchedBody>) q.getResultList();
            for (MatchedBody item : pagedResult) {
                MatchedBody emptyMatchedBody = getEmptyInstance();
                emptyMatchedBody.setId(item.getId());
                emptyMatchedBody.setGroupId(item.getGroupId());
                result.add(emptyMatchedBody);
            }
            entityManager.clear();
            page++;
        }

        return result;
    }

    @Override
    public final List<MatchedGroupInfo> getGroupsInfo(final List<String> groups) {
        if (groups == null || groups.isEmpty()) {
            return Collections.emptyList();
        }

        final StringBuilder gidStm = new StringBuilder();
            groups.forEach((gid) -> {
                if (gidStm.length() > 0) {
                    gidStm.append(" or ");
                }
                gidStm.append("data @> '{\"groupId\":\"").append(gid).append("\"}'");
            });


        Query q = entityManager.createNativeQuery(
            "select data#>>'{groupId}' as groupId,"
                + "SUM((data @> '{\"bodyIds\": [{\"type\": \"ETALON_ID\"}]}')::int) > 0 as hasEtalon,"
                + "COUNT(*) as size"
                + " FROM {h-schema}matched_body"
                + " WHERE modifiedby = '" + workerName + "' AND modifiedbyversion = '" + workerVersion + "'"
                + " AND (" + gidStm.toString() + ")"
                + " GROUP BY gid ORDER BY size DESC", MatchedGroupInfo.class);


        return (List<MatchedGroupInfo>) deserialize(q.getResultList());
    }

	@Override
	public final List<String> getEtalonGroupIds() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public final Map<String, String> getHashAndGroupIds() {
		// TODO Auto-generated method stub
		return null;
	}
}
