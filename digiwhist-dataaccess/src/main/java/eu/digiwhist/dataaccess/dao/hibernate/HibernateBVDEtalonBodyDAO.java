package eu.digiwhist.dataaccess.dao.hibernate;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import javax.persistence.EntityManager;
import javax.persistence.Query;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import eu.digiwhist.dataaccess.dto.matched.BVDEtalonBody;
import eu.dl.dataaccess.dao.EtalonBodyDAO;
import eu.dl.dataaccess.dao.hibernate.HibernateTransactionUtils;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.matched.MatchedGroupInfo;

/**
 * Hibernate DAO implementation for BvD etalon bodies.
 * 
 * @author Tomas Mrazek
 */
public final class HibernateBVDEtalonBodyDAO implements EtalonBodyDAO<BVDEtalonBody> {

    private final Logger logger = LoggerFactory.getLogger(this.getClass());

    private final EntityManager entityManager;

    private static HibernateBVDEtalonBodyDAO instance;

    private static final int DIGEST_LENGTH_THRESHOLD = 3;
    
    /**
     * Initialize connection to db.
     */
    private HibernateBVDEtalonBodyDAO() {
        entityManager = HibernateTransactionUtils.getInstance().getEntityManager();
    }
        
    /**
     * Returns initialised utils.
     * 
     * @return utils
     */
    public static HibernateBVDEtalonBodyDAO getInstance() {
        if (instance == null) {
            instance = new HibernateBVDEtalonBodyDAO();
        }

        return instance;
    }
    
    @Override
    public BVDEtalonBody getById(final String id) {
        return entityManager.find(BVDEtalonBody.class, new BigInteger(id));
    }

    @Override
    public List<BVDEtalonBody> getExactMatchBodiesPool(final String standardizedName, final String standardizedAddress,
        final List<BodyIdentifier> bodyIds) {

        if (standardizedName == null && standardizedAddress == null && (bodyIds == null || bodyIds.isEmpty())) {
            return Collections.emptyList();
        }

        StringBuilder restriction = exactMatchRestrictionBuilder(standardizedName, standardizedAddress, bodyIds);

        if (restriction.length() == 0) {
            return Collections.emptyList();
        }

        Query q = entityManager.createNativeQuery(
            "SELECT * FROM bvd.registry_information WHERE " + restriction.toString(), BVDEtalonBody.class);

        return (List<BVDEtalonBody>) q.getResultList();
    }

    @Override
    public List<BVDEtalonBody> getApproximateMatchBodiesPool(final String standardizedName,
        final String standardizedAddress, final List<BodyIdentifier> bodyIds, final String digest) {

        if (digest == null || digest.length() <= DIGEST_LENGTH_THRESHOLD) {
            return Collections.emptyList();
        }

        StringBuilder restriction =
            approximateMatchRestrictionBuilder(standardizedName, standardizedAddress, bodyIds, digest);

        if (restriction.length() == 0) {
            return Collections.emptyList();
        }

        Query q = entityManager.createNativeQuery(
            "SELECT * FROM bvd.registry_information WHERE " + restriction.toString(), BVDEtalonBody.class);

        return (List<BVDEtalonBody>) q.getResultList();
    }

    @Override
    public List<BVDEtalonBody> findAll(final int pageNumber, final int pageSize) {
        if (pageNumber <= 0 || pageSize <= 0) {
            return Collections.emptyList();
        }
        
        Query q = entityManager.createNativeQuery("SELECT * FROM bvd.registry_information", BVDEtalonBody.class);
        q.setFirstResult((pageNumber - 1) * pageSize);
        q.setMaxResults(pageSize);

        return (List<BVDEtalonBody>) q.getResultList();
    }

    @Override
    public List<BVDEtalonBody> findAll(final int pageNumber, final int pageSize, final int offset) {
        // TODO Auto-generated method stub
        return null;
    }

    /**
     * Returns restriction for BvD etalon database query that returns etalons for exact matching. Null values of input
     * parameters aren't taken into account.
     *
     * @param standardizedName
     *      standardized name
     * @param standardizedAddress
     *      standardized address
     * @param bodyIds
     *      list of body identifiers
     * @return restriction as StringBuilder instance
     */
    private StringBuilder exactMatchRestrictionBuilder(final String standardizedName,
        final String standardizedAddress, final List<BodyIdentifier> bodyIds) {

        StringBuilder restriction = new StringBuilder();
        if (standardizedName != null) {
            restriction.append("standardizedname = '").append(sanitize(standardizedName)).append("'");
        }
        if (standardizedAddress != null) {
            restriction
                .append(restriction.length() > 0 ? " OR " : "")
                .append("standardizedaddress = '").append(sanitize(standardizedAddress)).append("'");
        }
        if (bodyIds != null) {
            StringBuilder bodyIdsRestriction = new StringBuilder();

            bodyIds.stream().forEach((id) -> {
                if (id.getId() != null && id.getScope() != null) {
                    StringBuilder fieldRestriction = new StringBuilder("=")
                        .append("'").append(sanitize(id.getId())).append("'")
                        .append(" AND country_iso_code='").append(id.getScope()).append("'");

                    Arrays.asList("vattax_number", "trade_register_number", "statistical_number").stream()
                        .forEach(f -> { bodyIdsRestriction
                            .append(bodyIdsRestriction.length() > 0 ? " OR " : "")
                            .append("(").append(f).append(fieldRestriction).append(")");
                    });
                }
            });

            if (bodyIdsRestriction.length() > 0) {
                restriction
                    .append(restriction.length() > 0 ? " OR " : "").append(bodyIdsRestriction);
            }
        }

        return restriction;
    }

    /**
     * Returns restriction for BvD etalon database query that returns etalons for approximate matching. Null values of
     * input parameters aren't taken into account.
     *
     * @param standardizedName
     *      standardized name
     * @param standardizedAddress
     *      standardized address
     * @param bodyIds
     *      list of body identifiers
     * @param digest
     *      digest
     * @return restriction as StringBuilder instance
     */
    private StringBuilder approximateMatchRestrictionBuilder(final String standardizedName,
        final String standardizedAddress, final List<BodyIdentifier> bodyIds, final String digest) {

        StringBuilder restriction = exactMatchRestrictionBuilder(standardizedName, standardizedAddress, bodyIds);

        if (digest != null) {
            restriction
                .append(restriction.length() > 0 ? " OR " : "")
                .append("digest = '").append(sanitize(digest)).append("'");
        }

        return restriction;
    }

    /**
     * Escapes string to be usable in statements.
     *
     * @param entry
     *            string to be sanitized
     * @return sanitized string
     */
    private String sanitize(final String entry) {
        return entry.replace("'", "\'");
    }

    @Override
    public void updateDigestsAndBodyIdsAndNuts(final BVDEtalonBody body) {
        // TODO Auto-generated method stub
    }
    
    @Override
    public List<MatchedGroupInfo> getGroupsInfo(final List<String> groups) {
        // for etalons applies that each etalon is in own group
         return groups.stream().map(n -> new MatchedGroupInfo().setGroupId(n).setHasEtalon(true).setSize(1))
            .collect(Collectors.toList());
    }

	@Override
	public List<BVDEtalonBody> findAllById(final int id, final int amount) {
		// TODO Auto-generated method stub
		return null;
	}
}
