package eu.digiwhist.dataaccess.dao.hibernate;

import eu.dl.dataaccess.dao.MatchedTenderDAO;
import eu.dl.dataaccess.dao.hibernate.GenericHibernateDAO;
import eu.dl.dataaccess.dto.matched.MatchedTender;

import javax.persistence.Query;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Hibernate DAO implementation for tenders.
 */
public class HibernateMatchedTenderDAO extends GenericHibernateDAO<MatchedTender> implements
        MatchedTenderDAO<MatchedTender> {

    @Override
    protected final Class<MatchedTender> getDTOClass() {
        return MatchedTender.class;
    }

    @Override
    public final MatchedTender getEmptyInstance() {
        return new MatchedTender();
    }

    @Override
    public final List<MatchedTender> getByGroupId(final String groupId) {
        Query q = entityManager.createQuery(
                "SELECT e FROM " + MatchedTender.class.getName() + " e WHERE groupId = :groupId");
        q.setParameter("groupId", groupId);

        List<MatchedTender> result = (List<MatchedTender>) q.getResultList();

        return deserialize(result);
    }

    @Override
    public final List<MatchedTender> getMineByHash(final String hash) {
        Query q = entityManager.createQuery(
                "SELECT e FROM " + getDTOClass().getName() + " e WHERE hash = :hash AND modifiedBy = :modifiedBy " +
                        "AND modifiedByVersion = :modifiedByVersion");

        q.setParameter("modifiedByVersion", workerVersion);
        q.setParameter("modifiedBy", workerName);
        q.setParameter("hash", hash);

        List<MatchedTender> result = (List<MatchedTender>) q.getResultList();

        return deserialize(result);
    }

    @Override
    public final List<MatchedTender> getByHash(final String hash) {
        String additionalMatchersRestriction = prepareAdditionalWorkersCondition();

        Query q = entityManager.createQuery(
                "SELECT e FROM " + getDTOClass().getName() + " e WHERE hash = :hash AND ( (modifiedBy = :modifiedBy "
                        + "AND modifiedByVersion = :modifiedByVersion) " + additionalMatchersRestriction + ")");

        q.setParameter("modifiedByVersion", workerVersion);
        q.setParameter("modifiedBy", workerName);
        q.setParameter("hash", hash);

        List<MatchedTender> result = (List<MatchedTender>) q.getResultList();

        return deserialize(result);
    }

    @Override
    public final List<MatchedTender> getByPublicationSourceIds(final List<String> publicationSourceIds) {
        if (publicationSourceIds == null || publicationSourceIds.isEmpty()) {
            return Collections.emptyList();
        }

        StringBuilder urlRestriction = new StringBuilder();
        for (String publicationSourceId : publicationSourceIds) {
            if (urlRestriction.length() > 0) {
                urlRestriction.append(" or ");
            }
            urlRestriction.append("data @> ")
                    .append("'{\"publications\":[{")
                    .append("\"sourceId\":\"")
                    .append(publicationSourceId)
                    .append("\"")
                    .append("}]}'");
        }

        String additionalMatchersRestriction = prepareAdditionalWorkersCondition();

        Query q = entityManager.createNativeQuery(
                "SELECT * FROM {h-schema}matched_tender WHERE ((modifiedBy = :modifiedBy " + "AND modifiedByVersion ="
                        + " :modifiedByVersion) " + additionalMatchersRestriction + ") AND (" + urlRestriction
                        .toString() + ")", MatchedTender.class);
        q.setParameter("modifiedBy", workerName);
        q.setParameter("modifiedByVersion", workerVersion);

        List<MatchedTender> result = (List<MatchedTender>) q.getResultList();
        return deserialize(result);
    }

    @Override
    public final List<MatchedTender> getByPublicationHumanReadableUrls(final List<URL> publicationHumanReadableUrls) {
        if (publicationHumanReadableUrls == null || publicationHumanReadableUrls.isEmpty()) {
            return Collections.emptyList();
        }

        StringBuilder urlRestriction = new StringBuilder();
        for (URL url : publicationHumanReadableUrls) {
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

        String additionalMatchersRestriction = prepareAdditionalWorkersCondition();

        Query q = entityManager.createNativeQuery(
                "SELECT * FROM {h-schema}matched_tender WHERE ((modifiedBy = :modifiedBy " + "AND modifiedByVersion ="
                        + " :modifiedByVersion) " + additionalMatchersRestriction + ") AND (" + urlRestriction
                        .toString() + ")", MatchedTender.class);
        q.setParameter("modifiedBy", workerName);
        q.setParameter("modifiedByVersion", workerVersion);

        List<MatchedTender> result = (List<MatchedTender>) q.getResultList();
        return deserialize(result);
    }

    @Override
    public final List<MatchedTender> getByPublicationMachineReadableUrls(
            final List<URL> publicationMachineReadableUrls) {
        if (publicationMachineReadableUrls == null || publicationMachineReadableUrls.isEmpty()) {
            return Collections.emptyList();
        }

        StringBuilder urlRestriction = new StringBuilder();
        for (URL url : publicationMachineReadableUrls) {
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

        String additionalMatchersRestriction = prepareAdditionalWorkersCondition();

        Query q = entityManager.createNativeQuery(
                "SELECT * FROM {h-schema}matched_tender WHERE ((modifiedBy = :modifiedBy " + "AND modifiedByVersion ="
                        + " :modifiedByVersion) " + additionalMatchersRestriction + ") AND (" + urlRestriction
                        .toString() + ")", MatchedTender.class);
        q.setParameter("modifiedBy", workerName);
        q.setParameter("modifiedByVersion", workerVersion);

        List<MatchedTender> result = (List<MatchedTender>) q.getResultList();
        return deserialize(result);
    }

    @Override
    public final List<MatchedTender> getByDocumentsUrl(final URL documentsUrl) {
        // it is not implemented
        return new ArrayList<>();
    }

    @Override
    public final List<MatchedTender> getForResend(final String workerName, final String workerVersion) {
        Query countQuery = entityManager.createQuery(
                "SELECT count(*) FROM " + getDTOClass().getName() + " e WHERE modifiedBy = :modifiedBy AND " +
                        "modifiedByVersion = :modifiedByVersion");
        countQuery.setParameter("modifiedByVersion", workerVersion);
        countQuery.setParameter("modifiedBy", workerName);
        Long count = ((Long) countQuery.getSingleResult());
        Integer page = 0;
        List<MatchedTender> result = new ArrayList<>();

        while ((page * PAGE_SIZE) < count) {
            Query q = entityManager.createQuery(
                    "SELECT e FROM " + getDTOClass().getName() + " e WHERE modifiedBy = :modifiedBy AND " +
                            "modifiedByVersion = :modifiedByVersion");
            q.setParameter("modifiedByVersion", workerVersion);
            q.setParameter("modifiedBy", workerName);
            q.setFirstResult(page * PAGE_SIZE);
            q.setMaxResults(PAGE_SIZE);

            List<MatchedTender> pagedResult = (List<MatchedTender>) q.getResultList();
            for (MatchedTender item : pagedResult) {
                MatchedTender emptyMatchedBody = getEmptyInstance();
                emptyMatchedBody.setId(item.getId());
                emptyMatchedBody.setGroupId(item.getGroupId());
                result.add(emptyMatchedBody);
            }
            entityManager.clear();
            page++;
        }

        return result;
    }
}
